;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.shiro.realm

  (:gen-class
   :extends org.apache.shiro.realm.AuthorizingRealm
   :name czlab.wabbit.shiro.realm.JdbcRealm
   :init myInit
   :constructors {[] []}
   :exposes-methods { }
   :state myState)

  (:require [czlab.twisty.codec :as co :refer [pwd<>]]
            [czlab.wabbit.shiro.model :as mo]
            [czlab.wabbit.shiro.core :as sh]
            [czlab.horde.connect :as ht]
            [czlab.basal.log :as log]
            [czlab.basal.core :as c]
            [czlab.horde.core :as hc])

  (:import [org.apache.shiro.realm CachingRealm AuthorizingRealm]
           [org.apache.shiro.subject PrincipalCollection]
           [org.apache.shiro.authz
            AuthorizationInfo
            AuthorizationException]
           [org.apache.shiro.authc
            SimpleAccount
            AuthenticationToken
            AuthenticationException]
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
  (let [db (ht/dbapi<> sh/*jdbc-pool* sh/*meta-cache*)
        ;;pwd (.getCredentials token)
        user (.getPrincipal token)
        sql (ht/simple-sqlr db)]
    (when-some [acc (sh/findLoginAccount sql user)]
      (SimpleAccount. acc
                      (:passwd acc)
                      (.getName this)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -doGetAuthorizationInfo
  ""
  [^AuthorizingRealm this ^PrincipalCollection principals]
  (let [db (ht/dbapi<> sh/*jdbc-pool* sh/*meta-cache*)
        acc (.getPrimaryPrincipal principals)
        sql (ht/simple-sqlr db)]
    (c/do-with
      [rc (SimpleAccount. acc
                          (:passwd acc)
                          (.getName this))]
      (doseq [r (hc/dbGetM2M {:joined
                              ::mo/AccountRoles :with sql} acc)]
        (.addRole rc ^String (:name r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn -init [] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


