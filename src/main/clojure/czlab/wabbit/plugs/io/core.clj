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

  (:use [czlab.wabbit.base.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.flux.wflow.core])

  (:import [czlab.wabbit.ctl Pluggable Pluglet PlugMsg]
           [czlab.flux.wflow Workstream Job Activity]
           [czlab.jasal Triggerable]
           [java.util Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- processOrphan ""
  ([job] (processOrphan job nil))
  ([^Job job ^Throwable e]
   (let [evt (.origin job)]
     (some-> e log/exception )
     (log/error (str "event [%s] "
                     "job#%s dropped") (gtid evt) (.id job)))))

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
  "" [^Pluglet co job e]
  (if-some+
    [r (strKW (:eror (.spec co)))]
    (.callEx (.. co server cljrt)
             r
             (vargs* Object job e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dispatch! ""
  ([evt] (dispatch! evt nil))
  ([^PlugMsg evt arg]
   (log/debug "[%s] event is disp!" (.. evt source id))
   (let
     [src (.source evt)
      cfg (.config src)
      c0 (strKW (:handler cfg))
      c1 (strKW (:handler arg))
      ctr (.server src)
      rts (.cljrt ctr)
      cb (stror c1 c0)
      job (job<> (.core ctr) nil evt)
      wf (or
           (try! (.call rts cb))
           (error! src job nil))]
     (log/debug
       (str "type = %s\n"
            "arg = %s\n"
            "cb = %s") (gtid src) arg c1 c0)
     (try
       (log/debug "job#%s => %s" (.id job) (.id src))
       (do->nil
         (cond
           (ist? Workstream wf)
           (.execWith ^Workstream wf job)
           (fn? wf)
           (doto (workstream<>
                   (script<> #(wf %2) nil))
             (.execWith job))
           :else
           (processOrphan job)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


