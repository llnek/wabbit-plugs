;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.test.wabbit.plugs.mock

  (:require [czlab.basal.scheduler :refer [scheduler<>]]
            [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.wabbit.base.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.io])

  (:import [czlab.wabbit.sys Execvisor Cljshim]
           [czlab.jasal Activable Schedulable]
           [czlab.wabbit.ctl PlugMsg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkexe
  ""
  ^Execvisor
  []
  (let
    [rts (Cljshim/newrt (getCldr) "mock")
     pid (juid)
     cpu (scheduler<> pid)
     impl (muble<> {:plugs {}})]
    (with-meta
      (reify
        Execvisor

        (pkeyBytes [this] (bytesify (.pkey this)))
        (pkey [_] "hello world")
        (homeDir [_] (getCwd))
        (cljrt [_] rts)

        (getx [_] impl)
        (version [_] "1.0")
        (id [_] pid)

        (uptimeInMillis [_] 0)
        (locale [_] nil)
        (startTime [_] 0)
        (kill9 [_] )
        (start [this _] )
        (stop [this] )

        (acquireDbPool [_ gid] nil)
        (acquireDbAPI [_ gid] nil)
        (dftDbPool [_] nil)
        (dftDbAPI [_] nil)

        (child [_ sid])
        (hasChild [_ sid])

        (core [_] cpu)
        (config [_] {})

        (dispose [this]
          (.dispose cpu)
          (.close rts)))

      {:typeid ::Execvisor})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- init
  ""
  [^Execvisor co]
  (let []
    (.activate ^Activable (.core co) nil) co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mocker
  ""
  ^Execvisor
  [_]
  (doto (mkexe) (init )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


