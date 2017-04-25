;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Core functions for all IO services."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.core

  (:require [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.base]
        [czlab.wabbit.xpis]
        [czlab.basal.core]
        [czlab.basal.meta]
        [czlab.basal.str])

  (:import [czlab.basal Cljrt]
           [czlab.jasal Triggerable]
           [java.util Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- processOrphan ""
  ([evt] (processOrphan evt nil))
  ([evt ^Throwable e]
   (let []
     (some-> e log/exception )
     (log/error (str "event [%s] "
                     "job#%s dropped") (gtid evt) (id?? evt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro s2ms
  "Convert seconds to milliseconds"
  {:no-doc true}
  [s]
  `(let [t# ~s] (if (spos?  t#) (* 1000 t#) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- error!
  "" [pglet evt e]
  (if-some+
    [r (strKW (get-in pglet
                      [:pspec :eror]))]
    (.callEx ^Cljrt
             (-> pglet
                 get-server cljrt) r (vargs* Object evt e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dispatch! ""
  ([evt] (dispatch! evt nil))
  ([evt arg]
   (let
     [plug (:source evt)
      cfg (:conf @plug)
      c0 (:handler cfg)
      c1 (:handler arg)
      cb (or c1 c0)
      _ (assert (ifn? cb))
      ^Cljrt rts (-> plug
                     get-server cljrt)
      wf (or (try! (.callVar rts cb))
             (error! src evt nil))]
     (log/debug "[%s] event is disp!" (id?? plug))
     (log/debug
       (str "source = %s\n"
            "arg = %s\n"
            "cb = %s") (gtid plug) arg cb)
     (try
       (log/debug "#%s => %s" (id?? evt) (id?? plug))
       (do->nil
         (if (fn? wf)
           (wf evt)
           (processOrphan evt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scheduleTrigger

  ""
  [^Timer timer
   ^Triggerable trig ^long millis]

  (if (and timer (spos? millis))
    (let [k (tmtask<> #(.fire trig))]
      (.schedule t k millis) (.setTrigger trig k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


