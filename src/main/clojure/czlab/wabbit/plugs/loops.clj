;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Basic functions for loopable services."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.loops

  (:require [czlab.basal.dates :refer [parseDate]]
            [czlab.basal.process :refer [async!]]
            [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.base]
        [czlab.wabbit.xpis]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.wabbit.plugs.core])

  (:import [czlab.jasal Hierarchical LifeCycle Idable]
           [java.util Date Timer TimerTask]
           [clojure.lang APersistentMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgRepeat
  "" ^TimerTask
  [^Timer timer [dw ds] ^long intv func]

  (log/info "Scheduling a *repeating* timer: %dms" intv)
  (if (spos? intv)
    (do-with [tt (tmtask<> func)]
      (if (ist? Date dw)
        (.schedule timer tt ^Date dw intv)
        (.schedule timer
                   tt
                   (long (if (spos? ds) ds 1000)) intv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgOnce
  "" ^TimerTask
  [^Timer timer [dw ds] func]

  (log/info "Scheduling a *one-shot* timer at %s" [dw ds])
  (do-with [tt (tmtask<> func)]
    (if (ist? Date dw)
      (.schedule timer tt ^Date dw)
      (.schedule timer
                 tt
                 (long (if (spos? ds) ds 1000))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cfgTimer
  "" ^TimerTask
  [timer wakeup {:keys [intervalSecs
                        delayWhen
                        delaySecs]} repeat?]
  (let [d [delayWhen (s2ms delaySecs)]]
    (if (and repeat?
             (spos? intervalSecs))
      (cfgRepeat timer
                 d
                 (s2ms intervalSecs) wakeup)
      (cfgOnce timer d wakeup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scheduleThreadedLoop "" [plug waker]
  (let [{:keys [intervalSecs] :as cfg}
        (:conf @plug)
        loopy (volatile! true)
        ms (s2ms intervalSecs)
        w #(async!
             (fn [_]
               (while @loopy
                 (waker plug) (pause ms))))]
    (cfgTimer (Timer. true) w cfg false)
    loopy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn stopThreadedLoop "" [loopy] (vreset! loopy false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-object TimerMsg
             PlugletMsg
             (get-pluglet [me] (:$source me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private evt<> "" [co repeat?]
  `(object<> TimerMsg
             {:id (str "TimerMsg." (seqint2))
              :$source ~co
              :tstamp (now<>) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable TimerPluglet
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me arg]
    (let [c (get-in @me [:pspec :conf])]
      (->> (prevarCfg (merge c arg))
           (setf! me :conf))))
  (start [me] (.start me nil))
  (start [me arg]
    (let [r? (:repeat? @me)]
      (->> (cfgTimer (Timer. true)
                     #(dispatch! (evt<> me r?))
                     (:conf @me) r?)
           (setf! me :ttask))))
  (stop [me]
    (cancelTimerTask (:ttask @me)))
  (dispose [me] (.stop me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  manyspecdef
  {:info {:name "Repeating Timer"
          :version "1.0.0"}
   :conf {:$pluggable ::RepeatingTimer
          :intervalSecs 300
          :delaySecs 0
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  onespecdef
  {:info {:name "One Shot Timer"
          :version "1.0.0"}
   :conf {:$pluggable ::OnceTimer
          :delaySecs 0
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxxTimer<> "" [_ id spec repeat?]
  (mutable<> TimerPluglet
             {:pspec (update-in spec
                                [:conf] expandVarsInForm)
              :parent _
              :emAlias id
              :repeat? repeat?}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn RepeatingTimerSpec "" ^APersistentMap [] manyspecdef)
(defn OnceTimerSpec "" ^APersistentMap [] onespecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn RepeatingTimer ""
  ([_ id] (RepeatingTimer _ id (RepeatingTimerSpec)))
  ([_ id spec] (xxxTimer<> _ id spec true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn OnceTimer ""
  ([_ id] (OnceTimer _ id (OnceTimerSpec)))
  ([_ id spec] (xxxTimer<> _ id spec false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

