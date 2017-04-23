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
        [czlab.flux.wflow]
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
  ([job] (processOrphan job nil))
  ([job ^Throwable e]
   (let [evt (rootage job)]
     (some-> e log/exception )
     (log/error (str "event [%s] "
                     "job#%s dropped") (gtid evt) (id?? job)))))

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
  "" [pglet job e]
  (if-some+
    [r (strKW (:eror (plug-spec pglet)))]
    (.callEx ^Cljrt
             (-> pglet
                 get-server cljrt) r (vargs* Object job e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dispatch! ""
  ([evt] (dispatch! evt nil))
  ([evt arg]
   (log/debug "[%s] event is disp!" (-> evt
                                        msg-source id??))
   (let
     [^Config src (msg-source evt)
      cfg (.config src)
      c0 (:handler cfg)
      c1 (:handler arg)
      ctr (get-server src)
      ^Cljrt rts (cljrt ctr)
      cb (or c1 c0)
      _ (assert (ifn? cb))
      job (job<> (scheduler ctr) nil evt)
      wf (or
           (try! (.callVar rts cb))
           (error! src job nil))]
     (log/debug
       (str "source = %s\n"
            "arg = %s\n"
            "cb = %s") (gtid src) arg cb)
     (try
       (log/debug "#%s => %s" (id?? job) (id?? src))
       (do->nil
         (cond
           (satisfies? Workstream wf)
           (exec-with wf job)
           (fn? wf)
           (-> (workstream<> (script<> #(wf %2) ))
               (exec-with job))
           :else
           (processOrphan job)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


