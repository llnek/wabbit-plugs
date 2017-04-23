;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for HTTP/MVC service."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.http

  (:require [czlab.basal.io :refer [dirReadWrite? xdata<> slurpUtf8]]
            [czlab.convoy.util :refer [parseBasicAuth]]
            [czlab.basal.format :refer [readEdn]]
            [czlab.twisty.codec :refer [passwd<>]]
            [czlab.basal.logging :as log]
            [czlab.wabbit.plugs.io.mvc :as mvc]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [czlab.wabbit.plugs.io.core]
        [czlab.nettio.discarder]
        [czlab.nettio.core]
        [czlab.wabbit.base]
        [czlab.convoy.core]
        [czlab.convoy.wess]
        [czlab.convoy.server]
        [czlab.convoy.routes]
        [czlab.flux.wflow]
        [czlab.twisty.ssl]
        [czlab.basal.core]
        [czlab.basal.io]
        [czlab.basal.str]
        [czlab.basal.meta])

  (:import [czlab.convoy HttpResult RouteCracker RouteInfo]
           [czlab.nettio WholeRequest InboundHandler]
           [java.nio.channels ClosedChannelException]
           [io.netty.handler.codec.http.websocketx
            TextWebSocketFrame
            WebSocketFrame
            BinaryWebSocketFrame]
           [io.netty.handler.codec DecoderException]
           [io.netty.handler.codec.http.cookie
            ServerCookieDecoder
            ServerCookieEncoder]
           [io.netty.bootstrap ServerBootstrap]
           [io.netty.buffer ByteBuf Unpooled]
           [io.netty.handler.ssl SslHandler]
           [clojure.lang Atom APersistentMap]
           [czlab.basal Cljrt]
           [java.util Timer TimerTask]
           [io.netty.handler.codec.http
            HttpResponseStatus
            HttpRequest
            HttpUtil
            HttpResponse
            DefaultHttpResponse
            FullHttpRequest
            HttpVersion
            HttpRequestDecoder
            HttpResponseEncoder
            DefaultCookie
            HttpHeaderValues
            HttpHeaderNames
            LastHttpContent
            HttpHeaders
            Cookie
            QueryStringDecoder]
           [java.io
            Closeable
            File
            IOException
            RandomAccessFile]
           [java.net
            HttpCookie
            URI
            URL
            InetAddress
            SocketAddress
            InetSocketAddress]
           [io.netty.channel
            Channel
            ChannelHandler
            ChannelFuture
            ChannelFutureListener
            ChannelPipeline
            ChannelHandlerContext
            SimpleChannelInboundHandler]
           [io.netty.handler.stream
            ChunkedStream
            ChunkedFile
            ChunkedInput
            ChunkedWriteHandler]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def ^:private ^String auth-token "authorization")
(def ^:private ^String basic-token "basic")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scanBasicAuth
  "Scan and parse if exists basic authentication"
  ^APersistentMap
  [evt] (if-some+ [v (msgHeader evt auth-token)] (parseBasicAuth v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeLoadRoutes
  "" [{:keys [routes]}]
  (when-not (empty? routes) (loadRoutes routes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeClose
  "" [evt cf] (closeCF cf (:isKeepAlive? @evt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resumeOnExpiry
  "" [^Channel ch evt]
  (try
    (->> (http-result ch evt HttpResponseStatus/INTERNAL_SERVER_ERROR)
         (reply-result ch))
    (catch ClosedChannelException _
      (log/warn "channel closed already"))
    (catch Throwable t# (log/exception t#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-object WsockMsg WsockMsgGist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsockEvent<> "" [co ch msg]
  (mutable<> WsockMsg
            (merge msg
                   {:id (str "WsockMsg." (seqint2))
                    :socket ch
                    :source co
                    :ssl? (maybeSSL? ch) })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable HttpEventObj
  HttpMsgGist
  Triggerable
  (setTrigger [me t]
    (setf! me  :trigger t))
  (cancel [me]
    (some-> (unsetf! me :trigger) cancelTimerTask))
  (fire [me _]
    (when-some [t (unsetf! me :trigger)]
      (setf! me :stale? true)
      (cancelTimerTask t)
      (resumeOnExpiry (:socket data) me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpEvent<> "" [co ch req]
  (let
    [{:keys [wantSession? session]}
     (.config ^Config co)
     {:keys [route cookies]} req]
    (->>
      {:session (if (and (!false? wantSession?)
                         (some-> ^IDeref
                                 (:info route)
                                 .deref :wantSession?))
                  (upstream (-> co get-server pkey-bytes)
                            cookies
                            (:macit? @session)))
       :source co
       :stale? false
       :id (str "HttpMsg." (seqint2))}
      (merge req)
      (mutable<> HttpEventObj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  "" [co {:keys [ch msg]}]
  (cond
    (satisfies? WsockMsgGist msg)
    (wsockEvent<> co ch msg)
    (satisfies? HttpMsgGist msg)
    (httpEvent<> co ch msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- boot! "" ^LifeCycle [co]
  (let
    [asset! (with-open [clj (Cljrt/newrt (getCldr))]
              (.varIt clj
                      "czlab.wabbit.plugs.io.mvc/asset!"))
     {:keys [waitMillis] :as cfg}
     (.config ^Config co)
     w (nettyWebServer<>)]
    (->>
      (fn [_]
        {:h1
         (proxy [InboundHandler][]
           (channelRead0 [ctx msg]
             (let [ev (evt<> co msg)
                   {:keys [static? handler]}
                   (some-> ^IDeref
                           (:info (:route @ev)) .deref)
                   hd (if (and static?
                               (nil? handler)) asset! handler)]
               (if (spos? waitMillis)
                 (hold-event co ev waitMillis))
               (dispatch! ev {:handler hd}))))})
      (assoc cfg :ifunc)
      (.init w))
    w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn processOrphan
  "" [job error]
  ;; 500 or 503
  (let [s (or (:statusCode @job) 500)
        evt (rootage job)
        ch (socket evt)]
    (->> (http-result ch evt s)
         (reply-result ch ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Discarder! "" [func arg]
  (let
    [^LifeCycle
     w (discardHTTPD<> func arg)] (.start w arg) #(.stop w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- basicfg

  "Basic http config"
  [pkey conf cfg0]

  (let [{:keys [serverKey
                port
                passwd] :as cfg}
        (merge conf cfg0)
        ssl? (hgl? serverKey)]
    (if ssl?
      (test-cond "server-key file url"
                 (. ^String serverKey startsWith "file:")))
    (->>
      {:port (if-not (spos? port) (if ssl? 443 80) port)
       :routes (maybeLoadRoutes cfg)
       :passwd (p-text (passwd<> passwd pkey))
       :serverKey (if ssl? (io/as-url serverKey))}
      (merge cfg ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  httpspecdef
  {:deps {:$auth [:czlab.wabbit.plugs.auth.core/WebAuth]}
   :eror :czlab.wabbit.plugs.io.http/processOrphan
   :info {:name "Web Site"
          :version "1.0.0"}
   :conf {:maxInMemory (* 1024 1024 4)
          :$pluggable ::HTTP
          :maxContentSize -1
          :waitMillis 0
          :sockTimeOut 0
          :host ""
          :port 9090
          :serverKey ""
          :passwd ""
          :errorHandler nil
          :handler nil
          :useETags? false
          :wsockPath ""
          ;;:wantSession? true
          :session {
            ;;4weeks
            :maxAgeSecs 2419200
            ;;1week
            :maxIdleSecs 604800
            :isHidden? true
            :sslOnly? false
            :macit? false
            :webAuth? true
            :domain ""
            :domainPath "/"
          }
          :wsite {
            :publicRootDir "${wabbit.user.dir}/public"
            :mediaDir "res"
            :pageDir "htm"
            :jsDir "jsc"
            :cssDir "css"
          }
          :routes
          [{:mount "res/{}" :uri "/(favicon\\..+)"}
           {:mount "{}" :uri "/public/(.*)"}
           {:uri "/?" :template  "main/index.html"}]}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTPSpec "" ^APersistentMap [] httpspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTP "" ^APersistentMap
  ([_ id] (HTTP _ id (HTTPSpec)))
  ([_ id spec]
   {:pspec (update-in spec
                      [:conf] expandVarsInForm)
    :id id
    :init
    (fn [me arg]
      (let [c (:conf (rvtbl (:vtbl @me) :pspec))
            k (-> me get-server pkey-chars)
            cfg (prevarCfg (basicfg k c arg))
            {:keys [publicRootDir pageDir]}
            (:wsite cfg)
            pub (io/file (str publicRootDir)
                         (str pageDir))]
        (when (dirReadWrite? pub)
          (log/debug "freemarker tpl root: %s" (fpath pub))
          (setf! me :ftlCfg (mvc/genFtlConfig {:root pub})))
        cfg))
    :start
    (fn [me _]
      (let [w (boot! me)]
        (setf! me :boot w)
        (.start w (.config ^Config me))))
    :stop
    (fn [me]
      (let [{:keys [boot]} @me]
        (some-> boot .stop)))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

