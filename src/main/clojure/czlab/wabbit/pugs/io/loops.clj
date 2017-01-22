;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Basic functions for loopable services."
      :author "Kenneth Leung"}

  czlab.wabbit.pugs.io.loops

  (:require [czlab.basal.dates :refer [parseDate]]
            [czlab.basal.process :refer [async!]]
            [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.base.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.wabbit.pugs.io.core])

  (:import [czlab.wabbit.pugs.io TimerMsg]
           [java.util Date Timer TimerTask]
           [clojure.lang APersistentMap]
           [czlab.wabbit.ctl Puglet Pluggable]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configRepeat
  ""
  [^Timer timer delays ^long intv func]
  (log/info "Scheduling a *repeating* timer: %dms" intv)
  (let
    [tt (tmtask<> func)
     [dw ds] delays]
    (if (spos? intv)
      (cond
        (inst? Date dw)
        (.schedule timer tt ^Date dw intv)
        :else
        (.schedule timer
                   tt
                   (long (if (> ds 0) ds 1000)) intv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configOnce
  ""
  [^Timer timer delays func]
  (log/info "Scheduling a *one-shot* timer at %s" delays)
  (let
    [tt (tmtask<> func)
     [dw ds] delays]
    (cond
      (inst? Date dw)
      (.schedule timer tt ^Date dw)
      :else
      (.schedule timer
                 tt
                 (long (if (> ds 0) ds 1000))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configTimer
  [timer wakeup {:keys [intervalSecs
                        delayWhen
                        delaySecs] :as cfg} repeat?]
  (let
    [d [delayWhen (s2ms delaySecs)]]
    (test-some "java-timer" timer)
    (if (and repeat?
             (spos? intervalSecs))
      (configRepeat timer
                    d
                    (s2ms intervalSecs) wakeup)
      (configOnce timer d wakeup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn threadedTimer
  ""
  [funcs]
  (let
    [wake (or (:wakeup funcs)
              (constantly nil))
     loopy (volatile! true)
     schedule
     (or (:schedule funcs)
         (fn [c]
           (async!
            #(while @loopy
               (wake)
               (pause (:intervalMillis c)))
            {:cl (getCldr)})))]
    (doto
      {:start
       (fn [cfg]
         (let
           [{:keys [intervalSecs
                    delaySecs delayWhen]} cfg
            func #(schedule {:intervalMillis
                            (s2ms intervalSecs)})]
           (if (or (spos? delaySecs)
                   (inst? Date delayWhen))
             (configOnce (Timer.)
                         [delayWhen (s2ms delaySecs)] func)
             (func))))
       :stop
       #(vreset! loopy false)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ^TimerMsg
  [co repeat?]
  (let [eeid (str "event#"
                  (seqint2))]
    (with-meta
      (reify
        TimerMsg
        (checkAuthenticity [_] false)
        (id [_] eeid)
        (source [_] co)
        (isRepeating [_] repeat?))
      {:typeid ::TimerMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  manyspecdef
  {:info {:name "Repeating Timer"
          :version "1.0.0"}
   :conf {:intervalSecs 300
          :delaySecs 0
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxxTimer<m>
  ""
  [co {:keys [conf] :as spec} repeat?]
  (let
    [tee (keyword (juid))
     impl (muble<>)
     stop #(do (try! (some-> ^Timer
                             (.getv impl tee) (.cancel)))
               (.unsetv impl tee))
     wakeup #(do (dispatch! (evt<> co repeat?))
                 (if-not repeat? (stop)))]
    (reify
      Pluggable
      (spec [_] manyspecdef)
      (config [_] (dissoc (.intern impl) tee))
      (init [_ arg]
        (.copyEx impl (merge conf arg)))
      (start [_ arg]
        (let [t (Timer. true)]
          (.setv impl tee t)
          (configTimer t wakeup (.intern impl) repeat?)))
      (stop [_] stop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  onespecdef
  {:info {:name "One Shot Timer"
          :version "1.0.0"}
   :conf {:delaySecs 0 :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxxTimer<1>
  ""
  [co {:keys [conf] :as spec} repeat?]
  (let
    [tee (keyword (juid))
     impl (muble<>)
     stop #(do (try! (some-> ^Timer
                             (.getv impl tee) (.cancel)))
               (.unsetv impl tee))
     wakeup #(do (dispatch! (evt<> co repeat?))
                 (if-not repeat? (stop)))]
    (reify
      Pluggable
      (spec [_] onespecdef)
      (config [_] (dissoc (.intern impl) tee))
      (init [_ arg]
        (.copyEx impl (merge conf arg)))
      (start [_ arg]
        (let [t (Timer. true)]
          (.setv impl tee t)
          (configTimer t wakeup (.intern impl) repeat?)))
      (stop [_] stop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn RepeatingTimer
  ""
  ^Pluggable
  [co spec]
  (xxxTimer<1> co spec true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn OnceTimer
  ""
  ^Pluggable
  [co spec]
  (xxxTimer<m> co spec false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


