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
<<<<<<< HEAD
(defn threadedVtbl "" []

  (let
    [loopy (volatile! true)]
    {:start
     (fn [vt co cfg]
       (let [{:keys [intervalSecs
                     delaySecs delayWhen]} cfg
             func #(rvtbl vt :schedule co {:intervalMillis
                                           (s2ms intervalSecs)})]
         (if (or (spos? delaySecs)
                 (ist? Date delayWhen))
           (configOnce (Timer.)
                       [delayWhen (s2ms delaySecs)] func)
           (func))))
     :schedule
     (fn [vt co c]
       (async!
         #(while @loopy
            (rvtbl vt :wake co)
            (pause (:intervalMillis c)))
         {:cl (getCldr)}))
     :wake (constantly nil)
     :stop (fn [_] (vreset! loopy false))}
=======
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
>>>>>>> 60cbd957690818fbfff04e68c59ea4623eed7a54

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defobject TimerMsg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<> [co repeat?]
  (object<> TimerMsg
            {:id (str "TimerMsg." (seqint2))
             :tstamp (now<>)
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
(defn- cancelTimer "" [^Timer t] (try! (some-> t .cancel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxxTimer<> "" [spec repeat?]

<<<<<<< HEAD
  (let
    [pspec (update-in spec
                      [:conf] expandVarsInForm)
     vtbl
     {:config (fn [me] (:conf @me))
      :init (fn [me arg]
              (let [c (get-in @me [:pspec :conf])]
                (alterStateful
                  me merge (prevarCfg (merge c arg)))))
      :start (fn [me arg]
               (let [t (Timer. true)
                     cfg (.config me)
                     w #(do (dispatch! (evt<> me repeat?))
                            (if-not repeat? (cancelTimer t)))]
                 (alterStateful me assoc :timer t)
                 (configTimer t w cfg repeat?)))
      :stop (fn [me] (cancelTimer (:timer @me)))}]
    (pluglet<> pspec vtbl)))
=======
  (let [pspec (update-in spec
                         [:conf] expandVarsInForm)
        vtbl
        {:stop #(try! (some-> ^Timer
                              (:timer (.deref %1)) .cancel))}]
    (object<> TimerMsg vbtl)))
>>>>>>> 60cbd957690818fbfff04e68c59ea4623eed7a54

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

