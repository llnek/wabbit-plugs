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
        [czlab.wabbit.base])

  (:import [java.net InetAddress ServerSocket Socket]
           [czlab.jasal LifeCycle Idable]
           [clojure.lang APersistentMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-object TcpMsg
  Disposable
  (dispose [me]
           (closeQ (:socket me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  "" [co ^Socket socket]
  (object<> TcpMsg
            {:sockout (.getOutputStream socket)
             :sockin (.getInputStream socket)
             :id (str "TcpMsg." (seqint2))
             :source co
             :socket socket}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sockItDown "" [co soc]
  (try!
    (log/debug "opened socket: %s" soc)
    (dispatch! (evt<> co soc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ssoc<>
  "" ^ServerSocket
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
(decl-mutable SocketPluglet
  Pluglet
  (hold-event [me t millis] (some-> (:timer @me)
                                    (scheduleTrigger t millis)))
  (get-server [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (start [me] (.start me nil))
  (start [me _]
    (when-some
      [ss (ssoc<> (:conf @me))]
      (setf! me :ssoc ss)
      (async!
        #(while (not (.isClosed ss))
           (try
             (sockItDown me (.accept ss))
             (catch Throwable t
               (let [m (.getMessage t)]
                 (if-not (and (hasNoCase? m "socket")
                              (hasNoCase? m "closed"))
                   (log/warn t "")))))))))
  (init [me arg]
    (let [c (get-in @me [:pspec :conf])]
      (doto->> (prevarCfg (merge c arg))
               (setf! me :conf))))
  (dispose [_])
  (stop [me]
    (closeQ (:ssoc @me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn SocketIO ""
  ([_ id] (SocketIO _ id (SocketIOSpec)))
  ([_ id spec]
   (mutable<> SocketPluglet
              {:pspec (update-in spec
                                 [:conf]
                                 expandVarsInForm)
               :parent _
               :emAlias id})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


