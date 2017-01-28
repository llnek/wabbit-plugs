;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for TCP socket service."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.socket

  (:require [czlab.basal.process :refer [async!]]
            [czlab.basal.meta :refer [getCldr]]
            [czlab.basal.io :refer [closeQ]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.plugs.io.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.wabbit.base.core])

  (:import [java.net InetAddress ServerSocket Socket]
           [czlab.wabbit.ctl Pluglet Pluggable]
           [clojure.lang APersistentMap]
           [czlab.wabbit.plugs.io SocketMsg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ""
  [co {:keys [^Socket socket]}]

  (let [eeid (str "event#" (seqint2))]
    (with-meta
      (reify SocketMsg
        (checkAuthenticity [_] false)
        (id [_] eeid)
        (sockOut [_] (.getOutputStream socket))
        (sockIn [_] (.getInputStream socket))
        (source [_] co)
        (dispose [_] (closeQ socket)))
      {:typeid ::SocketMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sockItDown
  ""
  [^Pluggable co soc]
  (try!
    (log/debug "opened socket: %s" soc)
    (dispatch! (evt<> (.parent co) {:socket soc}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ssoc<>
  ""
  ^ServerSocket
  [{:keys [timeoutMillis backlog host port]}]
  (let
    [ip (if (hgl? host)
          (InetAddress/getByName host)
          (InetAddress/getLocalHost))
     _ (test-pos "socket port" port)
     soc (ServerSocket. port
                        (int (or backlog 100)) ip)]
    (log/info "Server socket %s (bound?) %s" soc (.isBound soc))
    (.setReuseAddress soc true)
    soc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  specdef
  {:info {:name "TCP Socket Server"
          :version "1.0.0"}
   :conf {:$pluggable ::SocketIO
          :host ""
          :port 7551
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn SocketIOSpec "" ^APersistentMap [] specdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn SocketIO
  ""
  ^Pluggable
  ([_] (SocketIO _ (SocketIOSpec)))
  ([_ {:keys [conf] :as pspec}]
   (let
     [impl (muble<>)]
     (reify
       Pluggable
       (setParent [_ p] (.setv impl :$parent p))
       (parent [_] (.getv impl :$parent))
       (config [_] (dissoc (.intern impl)
                           :$parent :$ssoc))
       (spec [_] pspec)
       (init [_ arg]
         (.copyEx impl (merge conf arg)))
       (start [this _]
         (when-some
           [ss (ssoc<> (.intern impl))]
           (.setv impl :$ssoc ss)
           (async!
             #(while (not (.isClosed ss))
                (try
                  (sockItDown this (.accept ss))
                  (catch Throwable t
                    (let [m (.getMessage t)]
                      (if (and (hasNoCase? m "socket")
                               (hasNoCase? m "closed"))
                        nil
                        (log/warn t ""))))))
             {:cl (getCldr)})))
       (stop [_]
         (closeQ (.unsetv impl :$ssoc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


