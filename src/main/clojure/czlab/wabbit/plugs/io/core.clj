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
        [czlab.basal.consts]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.flux.wflow.core])

  (:import [czlab.wabbit.ctl Pluggable Pluglet PlugMsg]
           [czlab.flux.wflow WorkStream Job TaskDef]
           [czlab.jasal Triggerable]
           [java.util Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- processOrphan
  ""
  ^WorkStream
  [_]
  (workStream<>
    (script<>
      #(let [^Job job %2
             evt (.origin job)]
         (do->nil
           (log/error "event '%s' {job:#s} dropped"
                      (:typeid (meta evt)) (.id job)))))))

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
  ""
  [co job e]
  (log/exception e)
  (some-> (processOrphan job) (.execWith job)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dispatch!
  ""
  ([evt] (dispatch! evt nil))
  ([^PlugMsg evt arg]
   (log/debug "[%s] event is disp!" (.id (.source evt)))
   (let
     [src (.source evt)
      cfg (.config src)
      c0 (strKW (:handler cfg))
      c1 (strKW (:router arg))
      ctr (.server src)
      rts (.cljrt ctr)
      cb (stror c1 c0)
      job (job<> (.core ctr) nil evt)
      wf (try! (.call rts cb))]
     (log/debug (str "event type = %s\n"
                     "event opts = %s\n"
                     "event router = %s\n"
                     "io-handler = %s")
                (:typeid (meta src)) arg c1 c0)
     (try
       (log/debug "job#%s => %s" (.id job) (.id src))
       (.setv job evt-opts arg)
       (cond
         (inst? WorkStream wf)
         (do->nil (.execWith ^WorkStream wf job))
         (fn? wf)
         (do->nil (wf job))
         :else
         (throwBadArg "Want WorkStream, got %s" (class wf)))
       (catch Throwable _
         (error! src job _))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


