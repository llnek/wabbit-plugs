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

  (:use [czlab.wabbit.base]
        [czlab.wabbit.xpis]
        [czlab.nettio.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.io])

  (:import [czlab.jasal LifeCycle Activable Schedulable]
           [czlab.basal Cljrt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkexe "" []

  (let
    [rts (Cljrt/newrt (getCldr) "mock")
     pid (jid<>)
     cpu (scheduler<> pid)]

    (reify
      Execvisor

      (get-home-dir [_] (io/file (sysProp "wabbit.user.dir")))
      (uptime-in-millis [_] 0)
      (get-locale [_] nil)
      (get-start-time [_] 0)
      (kill9! [_] )
      (cljrt [_] rts)
      (get-child [_ sid])
      (has-child? [_ sid])
      (get-scheduler [_] cpu)

      KeyAccess
      (pkey-bytes [this] (bytesit "hello world"))
      (pkey-chars [_] (.toCharArray "hello world"))

      SqlAccess
      (acquire-db-pool [_ gid] nil)
      (acquire-db-api [_ gid] nil)
      (dft-db-pool [_] nil)
      (dft-db-api [_] nil)

      LifeCycle
      (start [this _] )
      (start [me])
      (init [_ _])
      (stop [this] )
      (dispose [this]
        (.dispose cpu)
        (.close rts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- init "" [co]
    (.activate ^Activable (get-scheduler co) ) co)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mocker "" ^LifeCycle [_] (doto (mkexe) init ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mockHttpMsg "" []

  (nettyMsg<> {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


