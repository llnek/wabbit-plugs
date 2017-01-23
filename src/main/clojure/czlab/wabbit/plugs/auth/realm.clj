;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.auth.realm

  (:gen-class
   :extends org.apache.shiro.realm.AuthorizingRealm
   :name czlab.wabbit.plugs.auth.realm.JdbcRealm
   :init myInit
   :constructors {[] []}
   :exposes-methods { }
   :state myState)

  (:require [czlab.twisty.codec :refer [passwd<>]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.plugs.auth.core]
        [czlab.horde.dbio.connect]
        [czlab.horde.dbio.core])

  (:import [org.apache.shiro.realm CachingRealm AuthorizingRealm]
           [org.apache.shiro.subject PrincipalCollection]
           [org.apache.shiro.authz
            AuthorizationInfo
            AuthorizationException]
           [org.apache.shiro.authc
            SimpleAccount
            AuthenticationToken
            AuthenticationException]
           [czlab.horde DbApi]
           [java.util Collection]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -myInit [] [ [] (atom nil) ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -doGetAuthenticationInfo
  ""
  [^AuthorizingRealm this ^AuthenticationToken token]
  (let [db (dbopen<+> *jdbc-pool* *meta-cache*)
        ;;pwd (.getCredentials token)
        user (.getPrincipal token)
        sql (.simpleSQLr db)]
    (try
      (when-some [acc (findLoginAccount sql user)]
        (SimpleAccount. acc
                        (:passwd acc)
                        (.getName this)))
      (finally
        (.finx db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -doGetAuthorizationInfo
  ""
  [^AuthorizingRealm this ^PrincipalCollection principals]
  (let [db (dbopen<+> *jdbc-pool* *meta-cache*)
        acc (.getPrimaryPrincipal principals)
        rc (SimpleAccount. acc
                           (:passwd acc)
                           (.getName this))
        sql (.simpleSQLr db)
        j :czlab.wabbit.auth.model/AccountRoles]
    (try
      (let [rs (dbGetM2M {:joined j :with sql} acc) ]
        (doseq [r rs]
          (.addRole rc ^String (:name r)))
        rc)
      (finally
        (.finx db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -init [] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


