;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.auth.core

  ;;(:gen-class)

  (:require [czlab.basal.format :refer [readEdn readJsonStr writeJsonStr]]
            [czlab.convoy.util :refer [generateCsrf filterFormFields]]
            [czlab.wabbit.plugs.io.http :refer [scanBasicAuth]]
            [czlab.horde.connect :refer [dbapi<>]]
            [czlab.basal.resources :refer [rstr]]
            [czlab.convoy.wess :as wss]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.plugs.auth.model]
        [czlab.twisty.codec]
        [czlab.wabbit.base]
        [czlab.convoy.core]
        [czlab.basal.core]
        [czlab.basal.io]
        [czlab.basal.meta]
        [czlab.basal.str]
        [czlab.horde.core])

  (:import [org.apache.shiro.authc.credential CredentialsMatcher]
           [org.apache.shiro.config IniSecurityManagerFactory]
           [org.apache.shiro.authc UsernamePasswordToken]
           [czlab.jasal XData I18N DataError]
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
  {email-param [ :email #(normalizeEmail %) ]
   captcha-param [ :captcha #(strim %) ]
   user-param [ :principal #(strim %) ]
   pwd-param [ :credential #(strim %) ]
   csrf-param [ :csrf #(strim %) ]
   nonce-param [ :nonce #(some? %) ]})

(def ^:dynamic *meta-cache* nil)
(def ^:dynamic *jdbc-pool* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol AuthPluglet
  ""
  (check-action [_ acctObj action] "")
  (login [_ user pwd] "")
  (add-account [_ options] "")
  (has-account? [_ options] "")
  (get-roles [_ acctObj] "")
  (get-account [_ options] ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn newSession<> "" [evt attrs]
  (let
    [plug (:source evt)
     s (wss/wsession<> (-> plug get-server pkey-bytes)
                       (:session (:conf @plug)))]
    (doseq [[k v] attrs] (set-session-attr s k v))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn csrfToken<> "Create or delete a csrf cookie"

  ;; if maxAge=-1, browser doesnt sent it back!
  [{:keys [domainPath domain]} token]

  (let [ok? (hgl? token)
        c (HttpCookie. wss/csrf-cookie
                       (if ok? token "*"))]
    (if (hgl? domainPath) (.setPath c domainPath))
    (if (hgl? domain) (.setDomain c domain))
    (.setHttpOnly c true)
    (. c setMaxAge (if ok? 3600 0))
    c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- crackFormFields
  "Parse a standard login-like form with userid,password,email"
  [evt]
  (if-some
    [itms (cast? czlab.convoy.upload.ULFormItems
                 (some-> ^XData (:body evt) .content))]
    (preduce<map>
      #(let [fm (get-field-name-lc %2)
             fv (.getString ^FileItem %2)]
         (log/debug "form-field=%s, value=%s" fm fv)
         (if-some [[k v] (get props-map fm)]
           (assoc! %1 k (v fv))
           %1))
      (filterFormFields itms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- crackBodyContent
  "Parse a JSON body"
  [evt]
  (let
    [xs (some-> ^XData (:body evt) .getBytes)
     json (-> (if xs
                (strit xs) "{}")
              (readJsonStr #(lcase %)))]
    (preduce<map>
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
    (preduce<map>
      #(let [[k [a1 a2]] props-map]
         (if (gistParam? evt k)
             (assoc! %1
                     a1
                     (a2 (gistParam evt k)))
             %1))
      props-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn maybeGetAuthInfo
  "Attempt to parse and get authentication info"
  ^APersistentMap
  [evt]
  (let []
    (if-some+
      [ct (msgHeader evt "content-type")]
      (cond
        (or (embeds? ct "form-urlencoded")
            (embeds? ct "form-data"))
        (crackFormFields evt)

        (embeds? ct "/json")
        (crackBodyContent evt)

        :else
        (crackParams evt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeDecodeField
  ""
  [info fld shiftCount]
  (if (:nonce info)
    (try!
      (let
        [decr (->> (get info fld)
                   (caesarDecrypt shiftCount))
         s (->> decr
                (.decode (Base64/getMimeDecoder))
                strit)]
        (log/debug "info = %s" info)
        (log/debug "decr = %s" decr)
        (log/debug "val = %s" s)
        (assoc info fld s)))
    info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getPodKey
  ""
  ^bytes
  [evt]
  (-> (:source evt) get-server pkey-bytes))

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

  (let [tbl (->> :czlab.wabbit.plugs.auth.model/LoginAccount
                 (get (:models *auth-meta-cache*))
                 dbtable)]
    (when-not (tableExist? pool tbl)
      (applyDDL pool)
      (if-not (tableExist? pool tbl)
        (dberr! (rstr (I18N/base)
                      "auth.no.table" tbl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getSQLr
  "" {:tag SQLr}
  ([ctr] (getSQLr ctr false))
  ([ctr tx?]
   {:pre [(some? ctr)]}
   (let [db (-> (dft-db-pool ctr)
                (dbapi<> *auth-meta-cache*))]
     (if tx?
       (composite-sqlr db)
       (simple-sqlr db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn createAuthRole
  "Create a new auth-role in db"
  ^APersistentMap
  [sql role desc]
  {:pre [(some? sql)]}
  (let [m (get (:models sql)
               :czlab.wabbit.plugs.auth.model/AuthRole)
        rc (-> (dbpojo<> m)
               (dbSetFlds* {:name role
                            :desc desc}))]
    (add-obj sql rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteAuthRole
  "Delete this role"
  [sql role]
  {:pre [(some? sql)]}
  (let [m (get (:models sql)
               :czlab.wabbit.plugs.auth.model/AuthRole)]
    (exec-sql sql
              (format
                "delete from %s where %s =?"
                (fmt-id sql (dbtable m))
                (fmt-id sql (dbcol :name m))) [(strim role)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listAuthRoles
  "List all the roles in db"
  [sql]
  {:pre [(some? sql)]}
  (find-all sql :czlab.wabbit.plugs.auth.model/AuthRole))

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
   {:pre [(some? sql)(hgl? user)]}

   (let [m (get (:models sql)
                :czlab.wabbit.plugs.auth.model/LoginAccount)
         ps (some-> pwdObj hashed)
         acc
         (->>
           (dbSetFlds* (dbpojo<> m)
                       (merge props {:acctid (strim user)
                                     :passwd ps}))
           (add-obj sql))]
     ;; currently adding roles to the account is not bound to the
     ;; previous insert. That is, if we fail to set a role, it's
     ;; assumed ok for the account to remain inserted
     (doseq [r roleObjs]
       (dbSetM2M {:joined :czlab.wabbit.plugs.auth.model/AccountRoles
                  :with sql} acc r))
     (log/debug "created new account %s%s%s%s"
                "into db: " acc "\nwith meta\n" (meta acc))
     acc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findLoginAccountViaEmail

  "Look for account with this email address"
  ^APersistentMap
  [sql email]
  {:pre [(some? sql)]}

  (find-one sql
            :czlab.wabbit.plugs.auth.model/LoginAccount
            {:email (strim email) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn findLoginAccount

  "Look for account with this user id"
  ^APersistentMap
  [sql user]
  {:pre [(some? sql)]}

  (find-one sql
            :czlab.wabbit.plugs.auth.model/LoginAccount
            {:acctid (strim user) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn getLoginAccount

  "Get the user account"
  [sql user pwd]

  (if-some
    [acct (findLoginAccount sql user)]
    (if (valid-hash? (pwd<> pwd)
                     (:passwd acct))
      acct
      (trap! GeneralSecurityException
             (rstr (I18N/base) "auth.bad.pwd")))
    (trap! GeneralSecurityException
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

  (let [m {:passwd (some-> pwdObj hashed)}]
    (->> (dbSetFlds*
           (mockPojo<> userObj) m)
         (mod-obj sql))
    (dbSetFlds* userObj m)))

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
      (->> (dbSetFlds*
             (mockPojo<> userObj) details)
           (mod-obj sql))
      (dbSetFlds* userObj details))
    userObj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteLoginAccountRole

  "Remove a role from this user"
  ^long
  [sql user role]
  {:pre [(some? sql)]}

  (dbClrM2M
    {:joined :czlab.wabbit.plugs.auth.model/AccountRoles :with sql} user role))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn addLoginAccountRole

  "Add a role to this user"
  ^APersistentMap
  [sql user role]
  {:pre [(some? sql)]}

  (dbSetM2M
    {:joined :czlab.wabbit.plugs.auth.model/AccountRoles :with sql} user role))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteLoginAccount

  "Delete this account"
  ^long
  [sql acctObj] {:pre [(some? sql)]} (del-obj sql acctObj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteUser

  "Delete the account with this user id"
  ^long
  [sql user]
  {:pre [(some? sql)]}

  (let [m (get (:models sql)
               :czlab.wabbit.plugs.auth.model/LoginAccount)]
    (exec-sql sql
              (format
                "delete from %s where %s =?"
                (fmt-id sql (dbtable m))
                (fmt-id sql (dbcol :acctid m))) [(strim user)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn listLoginAccounts

  "List all user accounts"
  [sql]
  {:pre [(some? sql)]}

  (find-all sql :czlab.wabbit.plugs.auth.model/LoginAccount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- initShiro
  ""
  [^File homeDir ^String podKey]

  (let [f (io/file homeDir "etc/shiro.ini")
        f (if-not (fileRead? f)
            (doto (io/file *tempfile-repo* (jid<>))
              (spit (resStr "czlab/wabbit/plugs/auth/shiro.ini")))
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
    [ck (get (:cookies evt) wss/csrf-cookie)
     csrf (some-> ck .getValue)
     info (try
            (getSignupInfo evt)
            (catch BadDataError _ {:e _}))
     rb (I18N/base)
     pa (-> (:source evt)
            (get-server )
            (get-child :$auth))]
    (log/debug "csrf = %s%s%s"
               csrf ", and form parts = " info)
    (test-some "auth-pluglet" pa)
    (cond
      (some? (:e info))
      (:e info)

      (and (hgl? challengeStr)
           (not= challengeStr (:captcha info)))
      (GeneralSecurityException. (rstr rb "auth.bad.cha"))

      (not= csrf (:csrf info))
      (GeneralSecurityException. (rstr rb "auth.bad.tkn"))

      (and (hgl? (:credential info))
           (hgl? (:principal info))
           (hgl? (:email info)))
      (if (has-account? pa info)
        (GeneralSecurityException. (str (:principal info)))
        (add-account pa info))

      :else
      (GeneralSecurityException. (rstr rb "auth.bad.req")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loginTestExpr<> "" [evt]
  (let
    [ck (get (:cookies evt) wss/csrf-cookie)
     csrf (some-> ck .getValue)
     info (try
            (getSignupInfo evt)
            (catch DataError _ {:e _}))
     rb (I18N/base)
     pa (-> (:source evt)
            (get-server )
            (get-child :$auth))]
    (log/debug "csrf = %s%s%s"
               csrf
               ", and form parts = " info)
    (test-some "auth-pluglet" pa)
    (cond
      (some? (:e info))
      (:e info)

      (not= csrf (:csrf info))
      (GeneralSecurityException. (rstr rb "auth.bad.tkn"))

      (and (hgl? (:credential info))
           (hgl? (:principal info)))
      (do-login pa
                (:principal info)
                (:credential info))

      :else
      (GeneralSecurityException. (rstr rb "auth.bad.req")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable AuthPlugletObj
  Pluglet
  (get-server [me] (:parent @me))
  (hold-event [_ _ _])
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me arg]
    (-> (get-server me)
        dft-db-pool assertPluginOK)
    (->> (merge (get-in @me
                        [:pspec :conf]) arg)
         (prevarCfg)
         (setf! me :conf))
    (initShiro (get-home-dir (get-server me))
               (pkey-chars (get-server me))))
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
          par (get-server me)
          pkey (pkey-chars par)]
      (createLoginAccount
        (getSQLr par)
        principal
        (pwd<> credential pkey)
        (dissoc arg :principal :credential) [])))
  (do-login [me u p]
    (binding
      [*meta-cache* *auth-meta-cache*
       *jdbc-pool* (dft-db-pool (get-server me))]
      (let
        [cur (SecurityUtils/getSubject)
         sss (.getSession cur)]
        (log/debug "Current user session %s" sss)
        (log/debug "Current user object %s" cur)
        (when-not (.isAuthenticated cur)
          (try!
            ;;(.setRememberMe token true)
            (.login cur
                    (UsernamePasswordToken. ^String u (strit p)))
            (log/debug "User [%s] logged in successfully" u)))
        (if (.isAuthenticated cur)
          (.getPrincipal cur)))))
  (has-account [me arg]
    (let [par (get-server me)
          pkey (pkey-chars par)]
      (hasLoginAccount? (getSQLr par)
                        (:principal arg))))
  (get-account [me arg]
    (let [{:keys [principal email]} arg
          par (get-server me)
          pkey (pkey-chars par)
          sql (getSQLr par)]
      (cond
        (hgl? principal)
        (findLoginAccount sql principal)
        (hgl? email)
        (findLoginAccountViaEmail sql email))))
  (get-roles [_ acct] []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn WebAuth "" [ctr id]
  (mutable<> AuthPlugletObj
             {:parent ctr
              :emAlias id }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftype PwdMatcher [] CredentialsMatcher

  (doCredentialsMatch [_ token info]
    (let [^AuthenticationToken tkn token
          ^AuthenticationInfo inf info
          pwd (.getCredentials tkn)
          uid (.getPrincipal tkn)
          pc (.getCredentials inf)
          tstPwd (pwd<> pwd)
          acc (-> (.getPrincipals inf)
                  .getPrimaryPrincipal)]
      (and (= (:acctid acc) uid)
           (.validateHash tstPwd pc)))))

(ns-unmap *ns* '->PwdMatcher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doMain "" [& args]
  (let [homeDir (io/file (first args))
        cmd (nth args 1)
        db (nth args 2)
        pod (slurpXXXConf homeDir cfg-pod-cf true)
        pkey (get-in pod [:info :digest])
        cfg (get-in pod [:rdbms (keyword db)])]
    (when cfg
      (let [pwd (p-text (pwd<> (:passwd cfg) pkey))
            j (dbspec<> (assoc cfg :passwd pwd))
            t (matchUrl (:url cfg))]
        (cond
          (= "init-db" cmd)
          (applyDDL j)

          (= "gen-sql" cmd)
          (if (> (count args) 3)
            (exportAuthPlugletDDL t
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


