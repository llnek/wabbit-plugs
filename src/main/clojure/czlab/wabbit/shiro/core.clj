;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.shiro.core

  ;;(:gen-class)

  (:require [czlab.basal.format :as f :refer [readEdn readJsonStr writeJsonStr]]
            [czlab.convoy.util :as ct :refer [generateCsrf]]
            [czlab.wabbit.plugs.http :as hp :refer [scanBasicAuth]]
            [czlab.basal.resources :as r :refer [rstr]]
            [czlab.convoy.wess  :as ss :refer :all]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [czlab.basal.log :as log]
            [czlab.wabbit.shiro.model :as mo]
            [czlab.twisty.codec :as co]
            [czlab.horde.connect :as ht]
            [czlab.wabbit.xpis :as xp]
            [czlab.wabbit.base :as b]
            [czlab.convoy.upload :as cu]
            [czlab.convoy.mime :as mi]
            [czlab.convoy.core :as cc]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [czlab.basal.meta :as m]
            [czlab.basal.str :as s]
            [czlab.horde.core :as hc])

  (:import [org.apache.shiro.authc.credential CredentialsMatcher]
           [org.apache.shiro.config IniSecurityManagerFactory]
           [org.apache.shiro.authc UsernamePasswordToken]
           [java.security GeneralSecurityException]
           [org.apache.commons.fileupload FileItem]
           [czlab.jasal
            LifeCycle
            Idable
            XData
            I18N
            DataError
            Hierarchical]
           [org.apache.shiro.realm AuthorizingRealm]
           [org.apache.shiro.subject Subject]
           [java.util Base64 Base64$Decoder]
           [org.apache.shiro SecurityUtils]
           [clojure.lang APersistentMap]
           [java.io File IOException]
           [org.apache.shiro.authz
            AuthorizationException
            AuthorizationInfo]
           [org.apache.shiro.authc
            SimpleAccount
            AuthenticationException
            AuthenticationToken
            AuthenticationInfo]
           [java.net HttpCookie]
           [java.util Properties]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(def ^:private ^String nonce-param "nonce_token")
(def ^:private ^String csrf-param "csrf_token")
(def ^:private ^String pwd-param "credential")
(def ^:private ^String email-param "email")
(def ^:private ^String user-param "principal")
(def ^:private ^String captcha-param "captcha")

;; hard code the shift position, the encrypt code
;; should match this value.
(def ^:private caesar-shift 13)

(def ^:private props-map
  {email-param [ :email #(mi/normalizeEmail %) ]
   captcha-param [ :captcha #(s/strim %) ]
   user-param [ :principal #(s/strim %) ]
   pwd-param [ :credential #(s/strim %) ]
   csrf-param [ :csrf #(s/strim %) ]
   nonce-param [ :nonce #(some? %) ]})

(def ^:dynamic *meta-cache* nil)
(def ^:dynamic *jdbc-pool* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol AuthPluglet
  ""
  (check-action [_ acctObj action] "")
  (do-login [_ user pwd] "")
  (add-account [_ options] "")
  (has-account? [_ options] "")
  (get-roles [_ acctObj] "")
  (get-account [_ options] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn newSession<> ""
  ([evt] (newSession<> evt nil))
  ([evt attrs]
   (let [plug (xp/get-pluglet evt)]
     (c/do-with
       [s (ss/wsession<> (-> plug xp/get-server xp/pkey-bytes)
                         (:session (:conf @plug)))]
       (doseq [[k v] attrs] (ss/set-session-attr s k v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn csrfToken<> "Create or delete a csrf cookie"

  ;; if maxAge=-1, browser doesnt sent it back!
  ^HttpCookie
  [{:keys [domainPath domain]} token]

  (c/do-with
    [c (HttpCookie. ss/*csrf-cookie*
                    (if (s/hgl? token) token "*"))]
    (if (s/hgl? domainPath) (.setPath c domainPath))
    (if (s/hgl? domain) (.setDomain c domain))
    (.setHttpOnly c true)
    (.setMaxAge c (if (s/hgl? token) 3600 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- crackFormFields
  "Parse a standard login-like form with userid,password,email"
  [evt]
  (if-some
    [itms (c/cast? czlab.convoy.upload.ULFormItems
                   (some-> ^XData (:body evt) .content))]
    (c/preduce<map>
      #(let [fm (cu/get-field-name-lc %2)
             fv (.getString ^FileItem %2)]
         (log/debug "form-field=%s, value=%s" fm fv)
         (if-some [[k v] (get props-map fm)]
           (assoc! %1 k (v fv))
           %1))
      (cu/filterFormFields itms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- crackBodyContent
  "Parse a JSON body"
  [evt]
  (let
    [xs (some-> ^XData (:body evt) .getBytes)
     json (-> (if xs
                (c/strit xs) "{}")
              (f/readJsonStr #(s/lcase %)))]
    (c/preduce<map>
      #(let [[k [a1 a2]] %2]
         (if-some [fv (get json k)]
           (assoc! %1 a1 (a2 fv))
           %1))
      props-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- crackParams
  "Parse form fields in the Url"
  [evt]
  (let []
    (c/preduce<map>
      #(let [[k [a1 a2]] props-map]
         (if (cc/gistParam? evt k)
             (assoc! %1
                     a1
                     (a2 (cc/gistParam evt k)))
             %1))
      props-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn maybeGetAuthInfo
  "Attempt to parse and get authentication info"
  ^APersistentMap
  [evt]
  (let []
    (c/if-some+
      [ct (cc/msgHeader evt "content-type")]
      (cond
        (or (s/embeds? ct "form-urlencoded")
            (s/embeds? ct "form-data"))
        (crackFormFields evt)

        (s/embeds? ct "/json")
        (crackBodyContent evt)

        :else
        (crackParams evt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeDecodeField
  ""
  [info fld shiftCount]
  (if (:nonce info)
    (c/try!
      (let
        [^String decr (->> (get info fld)
                           (co/decrypt (co/caesar<>) shiftCount))
         s (->> decr
                (.decode (Base64/getMimeDecoder))
                c/strit)]
        (log/debug "info = %s" info)
        (log/debug "decr = %s" decr)
        (log/debug "val = %s" s)
        (assoc info fld s)))
    info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getPodKey
  ""
  ^bytes
  [evt]
  (-> evt xp/get-pluglet xp/get-server xp/pkey-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private getXXXInfo
  ""
  [evt]
  `(-> (maybeGetAuthInfo ~evt)
       (maybeDecodeField :principal caesar-shift)
       (maybeDecodeField :credential caesar-shift)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getSignupInfo "" [evt] (getXXXInfo evt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getLoginInfo "" [evt] (getXXXInfo evt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- assertPluginOK

  "If the plugin has been initialized,
   by looking into the db"
  [pool]
  {:pre [(some? pool)]}

  (let [tbl (->> ::mo/LoginAccount
                 (get (:models mo/*auth-meta-cache*))
                 hc/dbtable)]
    (if-not
      (hc/tableExist? pool tbl)
      (mo/applyDDL pool))
    (if (hc/tableExist? pool tbl)
      (log/info "czlab.wabbit.shiro.model* - ok")
      (hc/dberr! (r/rstr (I18N/base)
                         "auth.no.table" tbl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getSQLr ""
  ([ctr] (getSQLr ctr false))
  ([ctr tx?]
   {:pre [(some? ctr)]}
   (let [db (-> (xp/dft-db-pool ctr)
                (ht/dbapi<> mo/*auth-meta-cache*))]
     (if tx?
       (ht/composite-sqlr db)
       (ht/simple-sqlr db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn createAuthRole
  "Create a new auth-role in db"
  ^APersistentMap
  [sql role desc]
  {:pre [(some? sql)]}
  (let [m (get (:models sql) ::mo/AuthRole)
        rc (-> (hc/dbpojo<> m)
               (hc/dbSetFlds* {:name role
                               :desc desc}))]
    (hc/add-obj sql rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteAuthRole
  "Delete this role"
  [sql role]
  {:pre [(some? sql)]}
  (let [m (get (:models sql) ::mo/AuthRole)]
    (hc/exec-sql sql
                 (format
                   "delete from %s where %s =?"
                   (hc/fmt-id sql (hc/dbtable m))
                   (hc/fmt-id sql (hc/dbcol :name m))) [(s/strim role)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listAuthRoles
  "List all the roles in db"
  [sql]
  {:pre [(some? sql)]}
  (hc/find-all sql ::mo/AuthRole))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn createLoginAccount

  "Create a new account
   props : extra properties, such as email address.
   roleObjs : a list of roles to be assigned to the account"
  {:tag APersistentMap}

  ([sql user pwdObj props]
   (createLoginAccount sql user pwdObj props nil))

  ([sql user pwdObj]
   (createLoginAccount sql user pwdObj nil nil))

  ([sql user pwdObj props roleObjs]
   {:pre [(some? sql)(s/hgl? user)]}
   (let [m (get (:models sql)
                ::mo/LoginAccount)
         ps (some-> pwdObj co/hashed)]
     (c/do-with
       [acc
        (->>
          (hc/dbSetFlds* (hc/dbpojo<> m)
                         (merge props {:acctid (s/strim user)
                                       :passwd ps}))
          (hc/add-obj sql))]
       ;; currently adding roles to the account is not bound to the
       ;; previous insert. That is, if we fail to set a role, it's
       ;; assumed ok for the account to remain inserted
       (doseq [r roleObjs]
         (hc/dbSetM2M {:joined ::mo/AccountRoles
                       :with sql} acc r))
       (log/debug "created new account %s%s%s%s"
                  "into db: " acc "\nwith meta\n" (meta acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findLoginAccountViaEmail

  "Look for account with this email address"
  ^APersistentMap
  [sql email]
  {:pre [(some? sql)]}

  (hc/find-one sql
               ::mo/LoginAccount
               {:email (s/strim email) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findLoginAccount

  "Look for account with this user id"
  ^APersistentMap
  [sql user]
  {:pre [(some? sql)]}

  (hc/find-one sql
               ::mo/LoginAccount
               {:acctid (s/strim user) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getLoginAccount

  "Get the user account"
  [sql user pwd]

  (if-some
    [acct (findLoginAccount sql user)]
    (if (co/valid-hash? (co/pwd<> pwd)
                        (:passwd acct))
      acct
      (c/trap! GeneralSecurityException
               (r/rstr (I18N/base) "auth.bad.pwd")))
    (c/trap! GeneralSecurityException
             (str "UnknownUser " user))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hasLoginAccount?
  "If this user account exists"
  [sql user] (some? (findLoginAccount sql user)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn changeLoginAccount

  "Change the account password"
  ^APersistentMap
  [sql userObj pwdObj]
  {:pre [(some? sql)
         (map? userObj)(some? pwdObj)]}

  (let [m {:passwd (some-> pwdObj co/hashed)}]
    (->> (hc/dbSetFlds*
           (hc/mockPojo<> userObj) m)
         (hc/mod-obj sql))
    (hc/dbSetFlds* userObj m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn updateLoginAccount

  "Update account details
   details: a set of properties such as email address"
  ^APersistentMap
  [sql userObj details]
  {:pre [(some? sql)(map? userObj)]}

  (if-not (empty? details)
    (do
      (->> (hc/dbSetFlds*
             (hc/mockPojo<> userObj) details)
           (hc/mod-obj sql))
      (hc/dbSetFlds* userObj details))
    userObj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteLoginAccountRole

  "Remove a role from this user"
  ^long
  [sql user role]
  {:pre [(some? sql)]}

  (hc/dbClrM2M
    {:joined ::mo/AccountRoles :with sql} user role))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addLoginAccountRole

  "Add a role to this user"
  ^APersistentMap
  [sql user role]
  {:pre [(some? sql)]}

  (hc/dbSetM2M
    {:joined ::mo/AccountRoles :with sql} user role))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteLoginAccount

  "Delete this account"
  ^long
  [sql acctObj] {:pre [(some? sql)]} (hc/del-obj sql acctObj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteUser

  "Delete the account with this user id"
  ^long
  [sql user]
  {:pre [(some? sql)]}

  (let [m (get (:models sql)
               ::mo/LoginAccount)]
    (hc/exec-sql sql
                 (format
                   "delete from %s where %s =?"
                   (hc/fmt-id sql (hc/dbtable m))
                   (hc/fmt-id sql (hc/dbcol :acctid m))) [(s/strim user)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listLoginAccounts

  "List all user accounts"
  [sql]
  {:pre [(some? sql)]}

  (hc/find-all sql ::mo/LoginAccount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- initShiro
  ""
  [^File homeDir ^String podKey]

  (let [f (io/file homeDir "etc/shiro.ini")
        f (if-not (i/fileRead? f)
            (doto (io/file i/*tempfile-repo* (c/jid<>))
              (spit (c/resStr "czlab/wabbit/auth/shiro.ini")))
            f)]
    (-> (io/as-url f)
        str
        IniSecurityManagerFactory.
        .getInstance
        SecurityUtils/setSecurityManager)
    (log/info "created shiro security manager")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn signupTestExpr<>

  "Test component of a standard sign-up workflow"
  [^String challengeStr evt]

  (let
    [^HttpCookie
     ck (get (:cookies evt) ss/*csrf-cookie*)
     csrf (some-> ck .getValue)
     info (try
            (getSignupInfo evt)
            (catch DataError _ {:e _}))
     rb (I18N/base)
     pa (-> (xp/get-pluglet evt)
            (xp/get-server )
            (xp/get-child :$auth))]
    (log/debug "csrf = %s%s%s"
               csrf ", and form parts = " info)
    (c/test-some "auth-pluglet" pa)
    (cond
      (some? (:e info))
      (:e info)

      (and (s/hgl? challengeStr)
           (not= challengeStr (:captcha info)))
      (GeneralSecurityException. (r/rstr rb "auth.bad.cha"))

      (not= csrf (:csrf info))
      (GeneralSecurityException. (r/rstr rb "auth.bad.tkn"))

      (and (s/hgl? (:credential info))
           (s/hgl? (:principal info))
           (s/hgl? (:email info)))
      (if (has-account? pa info)
        (GeneralSecurityException. "DuplicateUser")
        (add-account pa info))

      :else
      (GeneralSecurityException. (r/rstr rb "auth.bad.req")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loginTestExpr<> "" [evt]
  (let
    [^HttpCookie
     ck (get (:cookies evt) ss/*csrf-cookie*)
     csrf (some-> ck .getValue)
     info (try
            (getSignupInfo evt)
            (catch DataError _ {:e _}))
     rb (I18N/base)
     pa (-> (xp/get-pluglet evt)
            (xp/get-server )
            (xp/get-child :$auth))]
    (log/debug "csrf = %s%s%s"
               csrf
               ", and form parts = " info)
    (c/test-some "auth-pluglet" pa)
    (cond
      (some? (:e info))
      (:e info)

      (not= csrf (:csrf info))
      (GeneralSecurityException. (r/rstr rb "auth.bad.tkn"))

      (and (s/hgl? (:credential info))
           (s/hgl? (:principal info)))
      (do-login pa
                (:principal info)
                (:credential info))

      :else
      (GeneralSecurityException. (r/rstr rb "auth.bad.req")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable AuthPlugletObj
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me arg]
    (-> (.parent me)
        xp/dft-db-pool assertPluginOK)
    (->> (merge (get-in @me
                        [:pspec :conf]) arg)
         (b/prevarCfg)
         (c/setf! me :conf))
    (initShiro (xp/get-home-dir (.parent me))
               (xp/pkey-chars (.parent me))))
  (start [me] (.start me nil))
  (start [me _]
    (log/info "AuthPluglet started"))
  (stop [me]
    (log/info "AuthPluglet stopped"))
  (dispose [_]
    (log/info "AuthPluglet disposed"))
  AuthPluglet
  (check-action [_ acctObj action] )
  (add-account [me arg]
    (let [{:keys [principal credential]} arg
          par (.parent me)
          pkey (xp/pkey-chars par)]
      (createLoginAccount
        (getSQLr par)
        principal
        (co/pwd<> credential pkey)
        (dissoc arg :principal :credential) [])))
  (do-login [me u p]
    (binding
      [*meta-cache* mo/*auth-meta-cache*
       *jdbc-pool* (xp/dft-db-pool (.parent me))]
      (let
        [cur (SecurityUtils/getSubject)
         sss (.getSession cur)]
        (log/debug "Current user session %s" sss)
        (log/debug "Current user object %s" cur)
        (when-not (.isAuthenticated cur)
          (c/try!
            ;;(.setRememberMe token true)
            (.login cur
                    (UsernamePasswordToken. ^String u (c/strit p)))
            (log/debug "User [%s] logged in successfully" u)))
        (if (.isAuthenticated cur)
          (.getPrincipal cur)))))
  (has-account? [me arg]
    (let [par (.parent me)
          pkey (xp/pkey-chars par)]
      (hasLoginAccount? (getSQLr par)
                        (:principal arg))))
  (get-account [me arg]
    (let [{:keys [principal email]} arg
          par (.parent me)
          pkey (xp/pkey-chars par)
          sql (getSQLr par)]
      (cond
        (s/hgl? principal)
        (findLoginAccount sql principal)
        (s/hgl? email)
        (findLoginAccountViaEmail sql email))))
  (get-roles [_ acct] []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn WebAuth "" [ctr id]
  (c/mutable<> AuthPlugletObj {:parent ctr :emAlias id }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype PwdMatcher [] CredentialsMatcher

  (doCredentialsMatch [_ token info]
    (let [^AuthenticationToken tkn token
          ^AuthenticationInfo inf info
          pwd (.getCredentials tkn)
          uid (.getPrincipal tkn)
          pc (.getCredentials inf)
          tstPwd (co/pwd<> pwd)
          acc (-> (.getPrincipals inf)
                  .getPrimaryPrincipal)]
      (and (= (:acctid acc) uid)
           (co/valid-hash? tstPwd pc)))))

(ns-unmap *ns* '->PwdMatcher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doMain "" [& args]
  (let [homeDir (io/file (first args))
        cmd (nth args 1)
        db (nth args 2)
        pod (b/slurpXXXConf homeDir b/cfg-pod-cf true)
        pkey (get-in pod [:info :digest])
        cfg (get-in pod [:rdbms (keyword db)])]
    (when cfg
      (let [pwd (co/p-text (co/pwd<> (:passwd cfg) pkey))
            j (hc/dbspec<> (assoc cfg :passwd pwd))
            t (hc/matchUrl (:url cfg))]
        (cond
          (= "init-db" cmd)
          (mo/applyDDL j)

          (= "gen-sql" cmd)
          (if (> (count args) 3)
            (mo/exportAuthPlugletDDL t
                                 (io/file (nth args 3)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; home gen-sql alias outfile
;; home init-db alias
(defn- main "Main Entry" [& args]
  ;; for security, don't just eval stuff
  ;;(alter-var-root #'*read-eval* (constantly false))
  (if-not (< (count args) 3) (apply doMain args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


