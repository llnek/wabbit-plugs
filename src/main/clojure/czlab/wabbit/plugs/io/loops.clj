;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Basic functions for loopable services."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.loops

  (:require [czlab.basal.dates :refer [parseDate]]
            [czlab.basal.process :refer [async!]]
            [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.base]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.wabbit.plugs.io.core])

  (:import [java.util Date Timer TimerTask]
           [clojure.lang APersistentMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configRepeat
  ""
  [^Timer timer [dw ds] ^long intv func]

  (log/info "Scheduling a *repeating* timer: %dms" intv)
  (let [tt (tmtask<> func)]
    (if (spos? intv)
      (cond
        (ist? Date dw)
        (.schedule timer tt ^Date dw intv)
        :else
        (.schedule timer
                   tt
                   (long (if (spos? ds) ds 1000)) intv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configOnce
  ""
  [^Timer timer [dw ds] func]

  (log/info "Scheduling a *one-shot* timer at %s" [dw ds])
  (let [tt (tmtask<> func)]
    (cond
      (ist? Date dw)
      (.schedule timer tt ^Date dw)
      :else
      (.schedule timer
                 tt
                 (long (if (spos? ds) ds 1000))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configTimer
  [timer wakeup {:keys [intervalSecs
                        delayWhen
                        delaySecs] :as cfg} repeat?]
  {:pre [(some? timer)]}
  (let [d [delayWhen (s2ms delaySecs)]]
    (if (and repeat?
             (spos? intervalSecs))
      (configRepeat timer
                    d
                    (s2ms intervalSecs) wakeup)
      (configOnce timer d wakeup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn threadedTimer "" [funcs]
  (let
    [wake (or (:wakeup funcs) (constantly nil))
     loopy (volatile! true)
     schedule
     (or (:schedule funcs)
         (fn [co c]
           (async!
            #(while @loopy
               (wake co)
               (pause (:intervalMillis c)))
            {:cl (getCldr)})))]
    {:start
     (fn [co cfg]
       (let [{:keys [intervalSecs
                     delaySecs delayWhen]} cfg
             func #(schedule %1 {:intervalMillis
                                (s2ms intervalSecs)})]
           (if (or (spos? delaySecs)
                   (ist? Date delayWhen))
             (configOnce (Timer.)
                         [delayWhen (s2ms delaySecs)] func)
             (func co))))
       :stop
       (fn [_] (vreset! loopy false))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defobject TimerMsg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<> [co repeat?]
  (object<> TimerMsg
            {:id (str "TimerMsg." (seqint2))
             :source co
             :isRepeating? repeat?} ))

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
(defentity xxxTimerPlug

  Pluggable
  (pluggableSpec [_] (:$pspec @data))

  Hierarchial
  (setParent [me p] (alterStateful me assoc :$parent p))
  (parent [_] (:$parent @data))

  Config
  (config [_] (dissoc @data :$timer :$parent))

  Initable
  (init [_ arg]
    (alterStateful me
                   merge
                   (prevarCfg (merge (:$conf @data) arg))))

  Startable
  (start [me arg]
    (let [t (Timer. true)
          pg (.parent me)
          vt @data
          w #(do (dispatch! (evt<> pg repeat?))
                 (if-not repeat?
                   (rvtbl vt :$stop me)))]
      (alterStateful me assoc :$timer t)
      (configTimer t w @data repeat?)))

  (stop [me] (rvtbl @data :$stop me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxxTimer<> "" [spec repeat?]

  (let [pspec (update-in spec
                         [:conf] expandVarsInForm)
        vtbl
        {:stop #(try! (some-> ^Timer
                              (:timer (.deref %1)) .cancel))}]
    (object<> TimerMsg vbtl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn RepeatingTimerSpec "" ^APersistentMap [] manyspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn RepeatingTimer "" ^czlab.wabbit.xpis.Pluglet
  ([_ id] (RepeatingTimer _ id (RepeatingTimerSpec)))
  ([_ id spec] (xxxTimer<> spec true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn OnceTimerSpec "" ^APersistentMap [] onespecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn OnceTimer "" ^czlab.wabbit.xpis.Pluglet

  ([_ id] (OnceTimer _ id (OnceTimerSpec)))
  ([_ id spec] (xxxTimer<> spec false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

