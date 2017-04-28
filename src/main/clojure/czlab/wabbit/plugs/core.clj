;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Core functions for all IO services."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.core

  (:require [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.base]
        [czlab.wabbit.xpis]
        [czlab.basal.core]
        [czlab.basal.meta]
        [czlab.basal.str])

  (:import [czlab.jasal Schedulable]
           [czlab.basal Cljrt]
           [java.util Timer TimerTask]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- processOrphan ""
  ([evt] (processOrphan evt nil))
  ([evt ^Throwable e]
   (let [plug (get-pluglet evt)]
     (some-> e log/exception )
     (log/error (str "event [%s] "
                     "%s dropped") (gtid evt) (id?? plug)))))

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
  "" [evt e]
  (let [plug (:source evt)]
    (if-some+
      [r (strKW (get-in @plug [:pspec :error]))]
      (.callEx ^Cljrt
               (-> plug
                   get-server cljrt) r (vargs* Object evt e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dispatch! ""
  ([evt] (dispatch! evt nil))
  ([evt arg]
   (let
     [pid (str "disp#" (seqint2))
      plug (get-pluglet evt)
      cfg (:conf @plug)
      ctr (get-server plug)
      sc (get-scheduler ctr)
      clj (cljrt ctr)
      dsp (:dispfn arg)
      h (or (:handler arg)
            (:handler cfg))
      f (if (var? h) @h)]
     (do->nil
       (log/debug "plug = %s\narg = %s\ncb = %s" (gtid plug) arg h)
       (log/debug "#%s => %s :is disp!" (id?? evt) (id?? plug))
       (if (fn? f)
         (->> (if (fn? dsp)
                (run-able+id<> pid (dsp f evt))
                (run-able+id<> pid (f evt)))
              (.run ^Schedulable sc ))
         (processOrphan evt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


