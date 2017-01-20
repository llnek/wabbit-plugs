;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.auth.model

  (:require [czlab.xlib.resources :refer [rstr]]
            [czlab.xlib.io :refer [spitUtf8]]
            [czlab.xlib.str :refer [toKW]]
            [czlab.xlib.logging :as log])

  (:use [czlab.horde.dbddl.postgresql]
        [czlab.horde.dbddl.sqlserver]
        [czlab.horde.dbddl.drivers]
        [czlab.horde.dbio.core]
        [czlab.horde.dbddl.h2]
        [czlab.horde.dbddl.mysql]
        [czlab.horde.dbddl.oracle])

  (:import [czlab.horde JdbcInfo JdbcPool Schema]
           [java.sql Connection]
           [java.io File]
           [czlab.xlib I18N]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:dynamic *auth-meta-cache*
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
        {:addr {:kind :O2O
                :cascade true
                :other ::StdAddress}})
      (dbuniques
        {:u2 #{:acctid}}))
    (dbjoined<> ::AccountRoles ::LoginAccount ::AuthRole)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genAuthPluginDDL
  "Generate db ddl for the auth-plugin"
  ^String
  [spec]
  {:pre [(keyword? spec)]}
  (if (contains? *db-types* spec)
    (getDDL *auth-meta-cache* spec)
    (dberr! (rstr (I18N/base)
                  "db.unknown"
                  (name spec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol PluginDDL "Upload the auth-plugin ddl to db" (applyDDL [_]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(extend-protocol PluginDDL

  JdbcInfo
  (applyDDL [this]
    (when-some [t (matchUrl (.url this))]
      (with-open [c (dbconnect<> this)]
        (uploadDdl c (genAuthPluginDDL t)))))

  JdbcPool
  (applyDDL [this]
    (when-some [t (matchUrl (.dbUrl this))]
      (uploadDdl this (genAuthPluginDDL t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn exportAuthPluginDDL
  "Output the auth-plugin ddl to file"
  [spec file]
  (spitUtf8 file (genAuthPluginDDL spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

