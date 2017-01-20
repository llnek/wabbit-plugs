;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Core functions for all IO services."
      :author "Kenneth Leung"}

  czlab.wabbit.io.core

  (:require [czlab.xlib.meta :refer [getCldr]]
            [czlab.xlib.logging :as log])

  (:use [czlab.wabbit.base.core]
        [czlab.xlib.consts]
        [czlab.xlib.core]
        [czlab.xlib.str]
        [czlab.flux.wflow.core])

  (:import [czlab.wabbit.io IoTrigger IoService IoEvent]
           [czlab.flux.wflow WorkStream Job TaskDef]
           [java.util Timer TimerTask]
           [czlab.wabbit.server
            Cljshim
            Container]
           [czlab.wabbit.pugs Pluggable]))

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
             evt (.event job)]
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
  ([^IoEvent evt arg]
   (log/debug "[%s] event is disp!" (.id (.source evt)))
   (let
     [src (.source evt)
      cfg (.config src)
      c1 (:router arg)
      c0 (:handler cfg)
      ctr (.server src)
      rts (.cljrt ctr)
      cb (stror c1 c0)
      job (job<> ctr nil evt)
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
;;
(defn service<>
  "Create a IO/Service"
  ^IoService
  [^Container parObj
   emType
   emAlias
   {:keys [info conf] :as spec}]
  (let [timer (atom nil)
        impl (atom nil)]
    (with-meta
      (reify IoService
        (setParent [_ p] (throwUOE "can't setParent"))
        (getx [_] (throwUOE "can't getx"))
        (isEnabled [_]
          (not (false? (:enabled? (.config ^Pluggable @impl)))))
        (server [this] (.parent this))
        (config [_] (.config ^Pluggable @impl))
        (hold [_ trig millis]
          (if (and (some? @timer)
                   (spos? millis))
            (let [k (tmtask<>
                      #(.fire trig nil))]
              (.schedule ^Timer @timer k millis)
              (.setTrigger trig k))))
        (version [_] (str (:version info)))
        (id [_] emAlias)
        (parent [_] parObj)
        (dispose [_]
          (log/info "io-service [%s] is being disposed" emAlias)
          (some-> ^Timer @timer (.cancel))
          (rset! timer)
          (.dispose ^Pluggable @impl)
          (log/info "io-service [%s] disposed - ok" emAlias))
        (init [this cfg0]
          (log/info "io-service [%s] is initializing..." emAlias)
          (let [c (-> (.cljrt parObj)
                      (.callEx (strKW emType)
                               (vargs* Object this spec)))]
            (rset! impl c)
            (.init ^Pluggable c cfg0))
          (log/info "io-service [%s] init'ed - ok" emAlias))
        (start [this arg]
          (log/info "io-service [%s] is starting..." emAlias)
          (rset! timer (Timer. true))
          (.start ^Pluggable @impl arg)
          (log/info "io-service [%s] config:" emAlias)
          (log/info "%s" (pr-str (.config this)))
          (log/info "io-service [%s] started - ok" emAlias))
        (stop [_]
          (log/info "io-service [%s] is stopping..." emAlias)
          (some-> ^Timer @timer (.cancel))
          (rset! timer)
          (.stop ^Pluggable @impl)
          (log/info "io-service [%s] stopped - ok" emAlias)))

      {:typeid emType})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


