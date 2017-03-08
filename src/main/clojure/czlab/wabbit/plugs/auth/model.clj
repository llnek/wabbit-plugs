;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.auth.model

  (:require [czlab.basal.resources :refer [rstr]]
            [czlab.basal.io :refer [spitUtf8]]
            [czlab.basal.str :refer [toKW]]
            [czlab.basal.logging :as log])

  (:use [czlab.horde.dbddl.postgresql]
        [czlab.horde.dbddl.sqlserver]
        [czlab.horde.dbddl.drivers]
        [czlab.horde.dbio.core]
        [czlab.horde.dbddl.h2]
        [czlab.horde.dbddl.mysql]
        [czlab.horde.dbddl.oracle])

  (:import [czlab.horde JdbcSpec JdbcPool Schema]
           [java.sql Connection]
           [java.io File]
           [czlab.jasal I18N]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:dynamic
  *auth-meta-cache*
  (dbschema<>
    (dbmodel<> ::StdAddress
      (dbfields
        {:addr1 {:size 255 :null false}
         :addr2 {}
         :state {:null false}
         :city {:null false}
         :zip {:null false}
         :country {:null false}})
      (dbindexes
        {:i1 #{:city :state :country}
         :i2 #{:zip :country}
         :state #{:state}
         :zip #{:zip}}))
    (dbmodel<> ::AuthRole
      (dbfields
        {:name {:column "role_name" :null false}
         :desc {:column "description" :null false}})
      (dbuniques
        {:u1 #{:name}}))
    (dbmodel<> ::LoginAccount
      (dbfields
        {:acctid {:null false}
         :email {:size 128}
          ;;:salt { :size 128}
         :passwd {:null false :domain :Password}})
      (dbassocs
        {:addr {:kind :o2o
                :cascade true
                :other ::StdAddress}})
      (dbuniques
        {:u2 #{:acctid}}))
    (dbjoined<> ::AccountRoles ::LoginAccount ::AuthRole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genAuthPlugletDDL

  "Generate db ddl for the auth-plugin"
  ^String
  [spec]
  {:pre [(keyword? spec)]}

  (if (contains? *db-types* spec)
    (getDdl *auth-meta-cache* spec)
    (dberr! (rstr (I18N/base) "db.unknown" (name spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol PlugletDDL "Upload the auth-pluglet ddl to db" (applyDDL [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(extend-protocol PlugletDDL

  JdbcSpec
  (applyDDL [this]
    (when-some [t (matchUrl (.url this))]
      (with-open [c (dbconnect<> this)]
        (uploadDdl c (genAuthPlugletDDL t)))))

  JdbcPool
  (applyDDL [this]
    (when-some [t (matchUrl (.dbUrl this))]
      (uploadDdl this (genAuthPlugletDDL t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn exportAuthPlugletDDL
  "Output the auth-plugin ddl to file"
  [spec file]
  (spitUtf8 file (genAuthPlugletDDL spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

