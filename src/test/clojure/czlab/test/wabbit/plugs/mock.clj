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

  (:require [czlab.basal.scheduler :as u :refer [scheduler<>]]
            [czlab.basal.meta :as m :refer [getCldr]]
            [czlab.basal.log :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [czlab.wabbit.base :as b]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s]
            [czlab.basal.io :as i]
            [czlab.wabbit.xpis :as xp]
            [czlab.nettio.core :as nc])

  (:import [czlab.jasal LifeCycle Activable Schedulable]
           [czlab.basal Cljrt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkexe "" []

  (let
    [rts (Cljrt/newrt (m/getCldr) "mock")
     pid (c/jid<>)
     cpu (u/scheduler<> pid)]

    (reify
      xp/Execvisor

      (get-home-dir [_] (io/file (c/sysProp "wabbit.user.dir")))
      (uptime-in-millis [_] 0)
      (get-locale [_] nil)
      (get-start-time [_] 0)
      (kill9! [_] )
      (cljrt [_] rts)
      (get-child [_ sid])
      (has-child? [_ sid])
      (get-scheduler [_] cpu)

      xp/KeyAccess
      (pkey-bytes [this] (c/bytesit "hello world"))
      (pkey-chars [_] (.toCharArray "hello world"))

      xp/SqlAccess
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
    (.activate ^Activable (xp/get-scheduler co) ) co)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mocker "" ^LifeCycle [_] (doto (mkexe) init ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mockHttpMsg "" []

  (nc/nettyMsg<> {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


