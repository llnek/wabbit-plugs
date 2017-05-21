;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for TCP socket service."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.socket

  (:require [czlab.basal.process :as p :refer [async!]]
            [czlab.basal.meta :as m :refer [getCldr]]
            [czlab.basal.io :as i :refer [closeQ]]
            [czlab.basal.log :as log]
            [czlab.wabbit.plugs.core :as pc]
            [czlab.wabbit.xpis :as xp]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s]
            [czlab.wabbit.base :as b])

  (:import [java.net InetAddress ServerSocket Socket]
           [clojure.lang APersistentMap]
           [czlab.jasal
            Disposable
            LifeCycle
            Idable
            Hierarchical]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-object TcpMsg
               xp/PlugletMsg
               (get-pluglet [me] (:$source me))
               Disposable
               (dispose [me]
                        (i/closeQ (:socket me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<> "" [co ^Socket socket]
  (c/object<> TcpMsg
              {:sockout (.getOutputStream socket)
               :sockin (.getInputStream socket)
               :id (str "TcpMsg." (c/seqint2))
               :$source co
               :socket socket}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sockItDown "" [co soc]
  (c/try!
    (log/debug "opened socket: %s" soc)
    (pc/dispatch! (evt<> co soc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ssoc<>
  "" ^ServerSocket
  [{:keys [timeoutMillis backlog host port]}]

  (let
    [ip (if (s/hgl? host)
          (InetAddress/getByName host)
          (InetAddress/getLocalHost))]
    (c/test-pos "socket port" port)
    (c/do-with
      [soc (ServerSocket. port
                          (int (or backlog 100)) ip)]
      (log/info "Server socket %s (bound?) %s" soc (.isBound soc))
      (.setReuseAddress soc true))))

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
(c/decl-mutable SocketPluglet
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (start [me] (.start me nil))
  (start [me _]
    (when-some
      [ss (ssoc<> (:conf @me))]
      (c/setf! me :ssoc ss)
      (p/async!
        #(while (not (.isClosed ss))
           (try
             (sockItDown me (.accept ss))
             (catch Throwable t
               (let [m (.getMessage t)]
                 (if-not (and (s/hasNoCase? m "socket")
                              (s/hasNoCase? m "closed"))
                   (log/warn t "")))))))))
  (init [me arg]
    (let [c (get-in @me [:pspec :conf])]
      (c/doto->> (b/prevarCfg (merge c arg))
                 (c/setf! me :conf))))
  (dispose [me] (.stop me))
  (stop [me]
    (i/closeQ (:ssoc @me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn SocketIO ""
  ([_ id] (SocketIO _ id (SocketIOSpec)))
  ([_ id spec]
   (c/mutable<> SocketPluglet
                {:pspec (update-in spec
                                   [:conf]
                                   b/expandVarsInForm)
                 :parent _
                 :emAlias id})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


