;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.shiro.model

  (:require [czlab.basal.resources :as r :refer [rstr]]
            [czlab.basal.io :as i :refer [spitUtf8]]
            [czlab.basal.str :as s :refer [toKW]]
            [czlab.basal.log :as log]
            [czlab.horde.drivers :as hd]
            [czlab.basal.core :as c]
            [czlab.horde.core :as hc])

  (:import [java.sql Connection]
           [java.io File]
           [czlab.jasal I18N]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:dynamic
  *auth-meta-cache*
  (hc/dbschema<>
    (hc/dbmodel<> ::StdAddress
      (hc/dbfields
        {:addr1 {:size 255 :null false}
         :addr2 {}
         :state {:null false}
         :city {:null false}
         :zip {:null false}
         :country {:null false}})
      (hc/dbindexes
        {:i1 #{:city :state :country}
         :i2 #{:zip :country}
         :state #{:state}
         :zip #{:zip}}))
    (hc/dbmodel<> ::AuthRole
      (hc/dbfields
        {:name {:column "role_name" :null false}
         :desc {:column "description" :null false}})
      (hc/dbuniques
        {:u1 #{:name}}))
    (hc/dbmodel<> ::LoginAccount
      (hc/dbfields
        {:acctid {:null false}
         :email {:size 128}
          ;;:salt { :size 128}
         :passwd {:null false :domain :Password}})
      (hc/dbassocs
        {:addr {:kind :o2o
                :cascade true
                :other ::StdAddress}})
      (hc/dbuniques
        {:u2 #{:acctid}}))
    (hc/dbjoined<> ::AccountRoles ::LoginAccount ::AuthRole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genAuthPlugletDDL

  "Generate db ddl for the auth-plugin"
  ^String
  [spec]
  {:pre [(keyword? spec)]}

  (if (c/in? hc/*db-types* spec)
    (hd/getDdl *auth-meta-cache* spec)
    (hc/dberr! (r/rstr (I18N/base) "db.unknown" (name spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn applyDDL "" [arg]
  (cond
    (c/ist? czlab.horde.core.JdbcSpec arg)
    (when-some [t (hc/matchUrl (:url arg))]
      (with-open [c (hc/dbconnect<> arg)]
        (hc/uploadDdl c (genAuthPlugletDDL t))))
    (c/ist? czlab.horde.core.JdbcPool arg)
    (when-some [t (hc/matchUrl (:url (:jdbc arg)))]
      (hc/uploadDdl arg (genAuthPlugletDDL t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn exportAuthPlugletDDL
  "Output the auth-plugin ddl to file"
  [spec file]
  (i/spitUtf8 file (genAuthPlugletDDL spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

