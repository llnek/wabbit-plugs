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
  "" [^Channel ch ^HttpMsg evt]
  (try
    (->> (httpResult<>
           evt
           HttpResponseStatus/INTERNAL_SERVER_ERROR)
         (.writeAndFlush ch )
         (maybeClose evt ))
    (catch ClosedChannelException _
      (log/warn "channel closed already"))
    (catch Throwable t# (log/exception t#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsockEvent<> "" [co ch msg]
  (let
    [body'
     (-> (or (some->
               (cast? BinaryWebSocketFrame msg)
               .content
               toByteArray)
             (some->
               (cast? TextWebSocketFrame msg) .text))
         xdata<> )
     ssl? (maybeSSL? ch)
     eeid (str "WsockMsg." (seqint2))]
    (reify WsockMsg
      (isBinary [_] (instBytes? (.content body')))
      (isText [_] (string? (.content body')))
      (socket [_] ch)
      (id [_] eeid)
      (isSSL [_] ssl?)
      (body [_] body')
      (source [_] co))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpEvent<>
  "" ^HttpMsg [^Pluglet co
               ^Channel ch ^WholeRequest req]
  (let
    [^InetSocketAddress laddr (.localAddress ch)
     body' (.content req)
     gs (.gist req)
     {:keys [wantSession? session]}
     (.config co)
     ^RouteInfo
     ri (get-in gs [:route :info])
     eeid (str "HttpMsg." (seqint2))
     cookieJar (:cookies gs)
     pkey (.. co server pkeyBytes)
     s? (and (!false? wantSession?)
             (some-> ri .wantSession))
     wss (if s?
           (->> (:macit? session)
                (upstream pkey cookieJar)))
     impl (muble<> {:$session wss :$stale? false})]
    (reify HttpMsg

      (session [_] (.getv impl :$session))
      (id [_] eeid)
      (source [_] co)
      (socket [_] ch)

      ;;:route {:redirect :status? :info :groups :places}
      (routeGist [_] (:route gs))

      (cookie [_ n] (get cookieJar n))
      (cookies [_] (vals cookieJar))
      (gist [_] gs)
      (body [_] body')

      (localAddr [_] (.. laddr getAddress getHostAddress))
      (localHost [_] (. laddr getHostName))
      (localPort [_] (. laddr getPort))

      (remotePort [_]
        (convLong (gistHeader gs "remote_port") 0))
      (remoteAddr [_]
        (str (gistHeader gs "remote_addr")))
      (remoteHost [_]
        (str (gistHeader gs "remote_host")))

      (serverPort [_]
        (convLong (gistHeader gs "server_port") 0))
      (serverName [_]
        (str (gistHeader gs "server_name")))

      (setTrigger [_ t] (.setv impl :$trigger t))
      (cancel [_]
        (some-> (.unsetv impl :$trigger)
                cancelTimerTask ))

      (fire [this _]
        (when-some [t (.unsetv impl :$trigger)]
          (.setv impl :$stale? true)
          (cancelTimerTask t)
          (resumeOnExpiry ch this)))

      (scheme [_] (if (:ssl? gs) "https" "http"))
      (isStale [_] (.getv impl :$stale?))
      (isSSL [_] (:ssl? gs))
      (getx [_] impl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  "" [co {:keys [ch msg]}]
  (if
    (ist? WebSocketFrame msg)
    (wsockEvent<> co ch msg)
    (httpEvent<> co ch msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- boot! "" [^Pluglet co]

  (let
    [asset! (with-open [clj (Cljrt/newrt (getCldr))]
              (.varIt clj
                      "czlab.wabbit.plugs.io.mvc/asset!"))
     {:keys [waitMillis] :as cfg}
     (.config co)
     bs
     (createServer<>
       :netty/http
       (fn [_]
         {:h1
          (proxy [InboundHandler][]
            (channelRead0 [ctx msg]
              (let
                [ev (evt<> co {:msg msg
                               :ch (ch?? ctx)})
                 ^RouteInfo
                 ri (some-> (cast? HttpMsg ev)
                            .routeGist
                            :info)
                 s? (some-> ri .isStatic)
                 hd (some-> ri .handler)
                 hd (if (and s? (nil? hd)) asset! hd)]
                (if (spos? waitMillis)
                  (.hold co ev waitMillis))
                (dispatch! ev {:handler hd}))))}) cfg)]
    [bs (startServer bs cfg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn processOrphan
  "" [^Job job error]
  ;; 500 or 503
  (let [s (or (.getv job :statusCode) 500)
        ^HttpMsg evt (.origin job)]
    (-> (httpResult<>
          evt
          (HttpResponseStatus/valueOf s))
        (replyResult ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Discarder! "" [func arg]
  (let
    [ch (-> (discardHTTPD<> func arg)
            (startServer arg))]
    #(stopServer ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- basicConfig

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
       :passwd (.text (passwd<> passwd pkey))
       :serverKey (if ssl? (io/as-url serverKey))}
      (merge cfg ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  httpspecdef
  {:eror :czlab.wabbit.plugs.io.http/processOrphan
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
(defn- httpXXX<>
  "" [{:keys [conf] :as pspec}]

  (let
    [impl (muble<>)]
    (reify Pluggable
      (setParent [_ p] (.setv impl :$parent p))
      (parent [_] (.getv impl :$parent))
      (config [_] (dissoc (.intern impl)
                          :$parent :$boot :$chan))
      (spec [_] (.getv impl :$pspec))
      (init [this arg]
        (let [^Pluglet pg (.parent this)
              h (.. pg server homeDir)
              k (.. pg server pkey)
              {{:keys [publicRootDir pageDir]}
               :wsite
               {:keys [webAuth?]}
               :session
               :as cfg}
              (basicConfig k conf arg)
              pub (io/file (str publicRootDir)
                           (str pageDir))]
          (.copyEx impl (prevarCfg cfg))
          (->> (if webAuth?
                 (assoc pspec
                        :deps
                        {:$auth [:czlab.wabbit.plugs.auth.core/WebAuth]})
                 pspec)
               (.setv impl :$pspec))
          (when (dirReadWrite? pub)
            (log/debug "freemarker tpl root: %s" (fpath pub))
            (.setv impl
                   :$ftlCfg
                   (mvc/genFtlConfig {:root pub})))))
      (start [this _]
        (let [[bs ch] (boot! (.parent this))]
          (.setv impl :$boot bs)
          (.setv impl :$chan ch)))
      (stop [_]
        (when-some
          [c (.unsetv impl :$chan)]
          (stopServer c))
        (.unsetv impl :$boot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTPSpec "" ^APersistentMap [] httpspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTP "" ^Pluggable
  ([_ id] (HTTP _ id (HTTPSpec)))
  ([_ id spec]
   (httpXXX<> (update-in spec [:conf] expandVarsInForm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

