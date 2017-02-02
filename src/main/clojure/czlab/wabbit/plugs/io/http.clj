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

  (:require [czlab.convoy.net.util :refer [parseBasicAuth]]
            [czlab.basal.io :refer [xdata<> slurpUtf8]]
            [czlab.basal.format :refer [readEdn]]
            [czlab.twisty.codec :refer [passwd<>]]
            [czlab.basal.logging :as log]
            [czlab.wabbit.plugs.io.mvc :as mvc]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [czlab.convoy.nettio.discarder]
        [czlab.convoy.nettio.core]
        [czlab.wabbit.plugs.io.core]
        [czlab.wabbit.base.core]
        [czlab.convoy.net.core]
        [czlab.convoy.net.wess]
        [czlab.convoy.net.server]
        [czlab.convoy.net.routes]
        [czlab.flux.wflow.core]
        [czlab.twisty.ssl]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.meta])

  (:import [czlab.convoy.net HttpResult RouteCracker RouteInfo]
           [czlab.convoy.nettio WholeRequest InboundHandler]
           [java.nio.channels ClosedChannelException]
           [io.netty.handler.codec.http.websocketx
            TextWebSocketFrame
            WebSocketFrame
            BinaryWebSocketFrame]
           [czlab.wabbit.ctl Pluggable Pluglet PlugMsg]
           [io.netty.handler.codec DecoderException]
           [czlab.wabbit.plugs.io HttpMsg WsockMsg]
           [io.netty.handler.codec.http.cookie
            ServerCookieDecoder
            ServerCookieEncoder]
           [io.netty.bootstrap ServerBootstrap]
           [io.netty.buffer ByteBuf Unpooled]
           [io.netty.handler.ssl SslHandler]
           [clojure.lang Atom APersistentMap]
           [czlab.flux.wflow Job]
           [czlab.wabbit.sys Execvisor]
           [czlab.twisty IPassword]
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
  [^HttpMsg evt]
  (if-some+ [v (-> (.msgGist evt)
                   (gistHeader auth-token))]
    (parseBasicAuth v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeLoadRoutes
  ""
  [{:keys [routes]}]
  (when-not (empty? routes) (loadRoutes routes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeClose
  ""
  [^HttpMsg evt ^ChannelFuture cf]
  (closeCF cf (:isKeepAlive? (.msgGist evt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resumeOnExpiry
  ""
  [^Channel ch ^HttpMsg evt]
  (try
    (->> (httpResult<>
           (.socket evt)
           (.msgGist evt)
           HttpResponseStatus/INTERNAL_SERVER_ERROR)
         (.writeAndFlush ch )
         (maybeClose evt ))
    (catch ClosedChannelException _
      (log/warn "channel closed already"))
    (catch Throwable t# (log/exception t#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsockEvent<>
  ""
  [co ch msg]
  (let
    [body'
     (-> (or (some->
               (cast? BinaryWebSocketFrame msg)
               (.content)
               (toByteArray))
             (some->
               (cast? TextWebSocketFrame msg)
               (.text)))
         (xdata<> ))
     ssl? (maybeSSL? ch)
     eeid (seqint2)]
    (with-meta
      (reify WsockMsg
        (isBinary [_] (instBytes? (.content body')))
        (isText [_] (string? (.content body')))
        (checkAuthenticity [_] false)
        (socket [_] ch)
        (id [_] eeid)
        (isSSL [_] ssl?)
        (body [_] body')
        (source [_] co))
      {:typeid ::WsockMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpEvent<>
  ""
  ^HttpMsg
  [^Pluglet co ^Channel ch ^WholeRequest req]
  (let
    [^InetSocketAddress laddr (.localAddress ch)
     body' (.content req)
     gist (.msgGist req)
     _ (log/debug "%s" gist)
     ^RouteInfo
     ri (get-in gist [:route :info])
     eeid (str "event#" (seqint2))
     cookieJar (:cookies gist)
     wss
     (upstream ri
               cookieJar
               (:ssl? gist)
               (.. co server pkeyBytes)
               (:maxIdleSecs (.config co)))
     impl (muble<> {:$session wss :$stale? false})]
    (with-meta
      (reify HttpMsg

        (checkAuthenticity [_] (some-> ri (.isSecure)))
        (checkSession [_] (some-> ri (.wantSession)))

        (session [_] (.getv impl :$session))
        (id [_] eeid)
        (source [_] co)
        (socket [_] ch)

        ;;:route {:redirect :status? :info :groups :places}
        (routeGist [_] (:route gist))

        (cookie [_ n] (get cookieJar n))
        (cookies [_] (vals cookieJar))
        (msgGist [_] gist)
        (body [_] body')

        (localAddr [_] (.. laddr getAddress getHostAddress))
        (localHost [_] (.getHostName laddr))
        (localPort [_] (.getPort laddr))

        (remotePort [_]
          (convLong (gistHeader gist "remote_port") 0))
        (remoteAddr [_]
          (str (gistHeader gist "remote_addr")))
        (remoteHost [_]
          (str (gistHeader gist "remote_host")))

        (serverPort [_]
          (convLong (gistHeader gist "server_port") 0))
        (serverName [_]
          (str (gistHeader gist "server_name")))

        (setTrigger [_ t] (.setv impl :$trigger t))
        (cancel [_]
          (some-> (.unsetv impl :$trigger)
                  (cancelTimerTask )))

        (fire [this _]
          (when-some [t (.unsetv impl :$trigger)]
            (.setv impl :$stale? true)
            (cancelTimerTask t)
            (resumeOnExpiry ch this)))

        (scheme [_] (if (:ssl? gist) "https" "http"))
        (isStale [_] (.getv impl :$stale?))
        (isSSL [_] (:ssl? gist))
        (getx [_] impl))

      {:typeid ::HTTPMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ""
  [co {:keys [ch msg]}]
  (let []
    (if
      (inst? WebSocketFrame msg)
      (wsockEvent<> co ch msg)
      (httpEvent<> co ch msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- boot!
  ""
  [^Pluglet co]
  (let
    [{:keys [waitMillis] :as cfg}
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
                            (.routeGist) (:info))
                 s? (some-> ri (.isStatic))
                 hd (some-> ri (.handler))
                 hd (if (and s? (nichts? hd))
                      "czlab.wabbit.plugs.io.mvc/asset!" hd)]
                (if (spos? waitMillis)
                  (.hold co ev waitMillis))
                (dispatch! ev {:handler hd}))))}) cfg)]
    [bs (startServer bs cfg)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn processOrphan
  ""
  [^Job job error]
  ;; 500 or 503
  (let [s (or (.getv job :statusCode) 500)
        ^HttpMsg evt (.origin job)]
    (->> (httpResult<>
           (.socket evt)
           (.msgGist evt)
           (HttpResponseStatus/valueOf s))
         (replyResult (.socket evt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Discarder!
  ""
  [func arg]
  (let
    [ch (-> (discardHTTPD<> func arg)
            (startServer arg))]
    #(stopServer ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpBasicConfig
  "Basic http config"
  [pkey conf cfg0]
  (let [{:keys [serverKey
                port
                passwd] :as cfg}
        (merge conf cfg0)
        kfile (expandVars serverKey)
        ssl? (hgl? kfile)]
    (if ssl?
      (test-cond "server-key file url"
                 (.startsWith kfile "file:")))
    (->>
      {:port (if-not (spos? port) (if ssl? 443 80) port)
       :routes (maybeLoadRoutes cfg)
       :passwd (.text (passwd<> passwd pkey))
       :serverKey (if ssl? (io/as-url kfile))}
      (merge cfg ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  httpspecdef
  {:eror :czlab.wabbit.plugs.io.http/processOrphan
   :info {:name "HTTP Server"
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
          :handler nil
          :routes nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpXXX<>
  ""
  [{:keys [conf] :as pspec}]
  (let
    [impl (muble<>)]
    (reify Pluggable
      (setParent [_ p] (.setv impl :$parent p))
      (parent [_] (.getv impl :$parent))
      (config [_] (dissoc (.intern impl)
                          :$parent :$boot :$chan))
      (spec [_] pspec)
      (init [this arg]
        (let [^Pluglet pg (.parent this)
              k (.. pg server pkey)]
          (.copyEx impl
                   (httpBasicConfig k conf arg))))
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
(defn loadTemplate
  ""
  ^APersistentMap
  [cfg tpath data]
  (let
    [ts (str "/" (triml tpath "/"))
     out (mvc/renderFtl cfg ts data)]
    {:data (xdata<> out)
     :ctype
     (cond
       (.endsWith ts ".json") "application/json"
       (.endsWith ts ".xml") "application/xml"
       (.endsWith ts ".html") "text/html"
       :else "text/plain")}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  mvcspecdef
  {:eror :czlab.wabbit.plugs.io.http/processOrphan
   :deps [:czlab.wabbit.plugs.auth.core/WebAuth]
   :info {:name "Web Site"
          :version "1.0.0"}
   :conf {:maxInMemory (* 1024 1024 4)
          :$pluggable ::WebMVC
          :maxContentSize -1
          :waitMillis 0
          :sockTimeOut 0
          :host ""
          :port 9090
          :serverKey ""
          :passwd ""
          :sessionAgeSecs 2592000
          :maxIdleSecs 0
          :hidden true
          :domain ""
          :domainPath "/"
          :maxAgeSecs 3600
          :useETags? false
          :errorHandler nil
          :handler nil
          :routes
          [{:mount "${pod.dir}/public/media/main/{}"
            :uri "/(favicon\\..+)"}
           {:mount "${pod.dir}/public/{}"
            :uri "/public/(.*)"}
           {:handler ""
            :uri "/?"
            :verbs #{:get}
            :template  "/main/index.html"}]}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpMVC<>
  ""
  [{:keys [conf] :as pspec}]
  (let
    [impl (muble<>)]
    (reify Pluggable
      (setParent [_ p] (.setv impl :$parent p))
      (parent [_] (.getv impl :$parent))
      (config [_] (dissoc (.intern impl)
                          :$ftlCfg :$parent :$boot :$chan))
      (spec [_] pspec)
      (init [this arg]
        (let [^Pluglet pg (.parent this)
              h (.. pg server homeDir)
              root (io/file h "public/pages")
              k (.. pg server pkey)]
          (.copyEx impl
                   (httpBasicConfig k conf arg))
          (.setv impl
                 :$ftlCfg
                 (mvc/genFtlConfig {:root root}))))
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
(defn WebMVCSpec "" ^APersistentMap [] mvcspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn WebMVC
  ""
  ^Pluggable
  ([_] (WebMVC _ (WebMVCSpec)))
  ([_ spec] (httpMVC<> spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTPSpec "" ^APersistentMap [] httpspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTP
  ""
  ^Pluggable
  ([_] (HTTP _ (HTTPSpec)))
  ([_ spec] (httpXXX<> spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


