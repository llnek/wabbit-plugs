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
   (let [plug (:source evt)]
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
      plug (:source evt)
      cfg (:conf @plug)
      ctr (get-server plug)
      sc (get-scheduler ctr)
      clj (cljrt ctr)
      h (or (:handler arg)
            (:handler cfg))
      v (try (.callVar ^Cljrt clj h)
             (catch Throwable _ (error! evt nil)))]
     (do->nil
       (log/debug "plug = %s\narg = %s\ncb = %s" (gtid plug) arg h)
       (log/debug "#%s => %s :is disp!" (id?? evt) (id?? plug))
       (if (fn? v)
         (.run ^Schedulable sc (run-able+id<> pid (v evt)))
         (processOrphan evt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


