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

  (:require [czlab.basal.meta :as m :refer [getCldr]]
            [czlab.basal.log :as log]
            [czlab.wabbit.base :as b]
            [czlab.wabbit.xpis :as xp]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

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
   (let [plug (xp/get-pluglet evt)]
     (some-> e log/exception )
     (log/error (str "event [%s] "
                     "%s dropped") (b/gtid evt) (c/id?? plug)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro s2ms
  "Convert seconds to milliseconds"
  {:no-doc true}
  [s]
  `(let [t# ~s] (if (czlab.basal.core/spos?  t#) (* 1000 t#) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- error!
  "" [evt e]
  (let [plug (:source evt)]
    (c/if-some+
      [r (s/strKW (get-in @plug [:pspec :error]))]
      (.callEx ^Cljrt
               (-> plug
                   xp/get-server xp/cljrt) r (c/vargs* Object evt e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn dispatch! ""
  ([evt] (dispatch! evt nil))
  ([evt arg]
   (let
     [pid (str "disp#" (c/seqint2))
      plug (xp/get-pluglet evt)
      cfg (:conf @plug)
      ctr (xp/get-server plug)
      sc (xp/get-scheduler ctr)
      clj (xp/cljrt ctr)
      dsp (:dispfn arg)
      h (or (:handler arg)
            (:handler cfg))
      f (if (var? h) @h)]
     (c/do->nil
       (log/debug "plug = %s\narg = %s\ncb = %s" (b/gtid plug) arg h)
       (log/debug "#%s => %s :is disp!" (c/id?? evt) (c/id?? plug))
       (if (fn? f)
         (->> (if (fn? dsp)
                (c/run-able+id<> pid (dsp f evt))
                (c/run-able+id<> pid (f evt)))
              (.run ^Schedulable sc ))
         (processOrphan evt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


