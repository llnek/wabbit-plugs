;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for HTTP/MVC service."
      :author "Kenneth Leung"}

  czlab.wabbit.pugs.io.http

  (:require [czlab.convoy.net.util :refer [parseBasicAuth]]
            [czlab.basal.io :refer [xdata<> slurpUtf8]]
            [czlab.basal.format :refer [readEdn]]
            [czlab.twisty.codec :refer [passwd<>]]
            [czlab.basal.logging :as log]
            [czlab.wabbit.pugs.mvc.ftl :as ftl]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [czlab.wabbit.base.core]
        [czlab.convoy.netty.discarder]
        [czlab.convoy.netty.server]
        [czlab.convoy.netty.routes]
        [czlab.convoy.netty.core]
        [czlab.convoy.net.core]
        [czlab.flux.wflow.core]
        [czlab.wabbit.pugs.io.core]
        [czlab.twisty.ssl]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.basal.meta])

  (:import [czlab.convoy.net HttpResult RouteCracker RouteInfo]
           [czlab.convoy.netty WholeRequest InboundHandler]
           [czlab.convoy.netty CPDecorator TcpPipeline]
           [java.nio.channels ClosedChannelException]
           [io.netty.handler.codec.http.websocketx
            TextWebSocketFrame
            WebSocketFrame
            BinaryWebSocketFrame]
           [io.netty.handler.codec.http.cookie
            ServerCookieDecoder
            ServerCookieEncoder]
           [io.netty.handler.codec DecoderException]
           [io.netty.bootstrap ServerBootstrap]
           [io.netty.buffer ByteBuf Unpooled]
           [io.netty.handler.ssl SslHandler]
           [czlab.flux.wflow Job]
           [czlab.wabbit.sys Execvisor]
           [czlab.wabbit.ctl Pluggable Puglet PugEvent]
           [clojure.lang Atom APersistentMap]
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
           [czlab.wabbit.pugs.io HttpMsg WSockMsg]
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
  [cfg]
  (let [{:keys [routes]} cfg]
    (when-not (empty? routes)
      (loadRoutes routes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- javaToCookie
  ""
  ^Cookie
  [^HttpCookie c]
  ;; stick with version 0, Java's HttpCookie defaults to 1 but that
  ;; screws up the Path attribute on the wire => it's quoted but
  ;; browser seems to not like it and mis-interpret it.
  ;; Netty's cookie defaults to 0, which is cool with me.
  (doto (DefaultCookie. (.getName c)
                        (.getValue c))
    ;;(.setComment (.getComment c))
    (.setDomain (.getDomain c))
    (.setMaxAge (.getMaxAge c))
    (.setPath (.getPath c))
    ;;(.setDiscard (.getDiscard c))
    (.setVersion 0)
    (.setHttpOnly (.isHttpOnly c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeClose
  ""
  [^HttpMsg evt ^ChannelFuture cf]
  (closeCF cf (:isKeepAlive? (.msgGist evt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- csToNetty
  ""
  [cookies]
  (preduce<vec>
    #(->> (.encode ServerCookieEncoder/STRICT ^Cookie %2)
          (conj! %1 ))
    (map #(javaToCookie %) (seq cookies))))

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
      (log/warn "closedChannelEx thrown"))
    (catch Throwable t# (log/exception t#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsockEvent<>
  ""
  [^Puglet co ^Channel ch ssl? msg]
  (let
    [_body
     (-> (cond
           (inst? BinaryWebSocketFrame msg)
           (-> (.content
                 ^BinaryWebSocketFrame msg)
               (toByteArray))
           (inst? TextWebSocketFrame msg)
           (.text ^TextWebSocketFrame msg))
         (xdata<>))
     eeid (seqint2)]
    (with-meta
      (reify WSockMsg
        (isBinary [_] (instBytes? (.content _body)))
        (isText [_] (string? (.content _body)))
        (checkAuthenticity [_] false)
        (socket [_] ch)
        (id [_] eeid)
        (isSSL [_] ssl?)
        (body [_] _body)
        (source [_] co))
      {:typeid ::WSockMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpEvent<>
  ""
  ^HttpMsg
  [^Puglet co ^Channel ch ssl? ^WholeRequest req]
  (let
    [^InetSocketAddress laddr (.localAddress ch)
     _body (.content req)
     gist (.msgGist req)
     rgist (:route gist)
     ^RouteInfo ri (:info rgist)
     wantSess? (some-> ri (.wantSession))
     wantSecure? (some-> ri (.isSecure))
     eeid (str "event#" (seqint2))
     cookieJar (:cookies gist)
     impl (muble<> {:stale false})]
    (with-meta
      (reify HttpMsg

        (checkAuthenticity [_] wantSecure?)
        (checkSession [_] wantSess?)
        (session [_] (.getv impl :session))
        (id [_] eeid)
        (source [_] co)
        (socket [_] ch)

        ;;:route {:redirect :status :info :groups :places }
        (routeGist [_] rgist)

        (cookie [_ n] (get cookieJar n))
        (cookies [_] (vals cookieJar))
        (msgGist [_] gist)
        (body [_] _body)

        (localAddr [_]
          (.getHostAddress (.getAddress laddr)))
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

        (setTrigger [_ t] (.setv impl :trigger t))
        (cancel [_]
          (if-some [t (.getv impl :trigger)]
            (cancelTimerTask t))
          (.unsetv impl :trigger))

        (fire [this _]
          (when-some [t (.getv impl :trigger)]
            (.setv impl :stale true)
            (.unsetv impl :trigger)
            (cancelTimerTask t)
            (resumeOnExpiry ch this)))

        (scheme [_] (if (:ssl? gist) "https" "http"))
        (isStale [_] (.getv impl :stale))
        (isSSL [_] ssl?)
        (getx [_] impl))
      {:typeid ::HTTPEvent})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ""
  [co {:keys [ch msg]}]
  (let [ssl? (maybeSSL? ch)]
    (if
      (inst? WebSocketFrame msg)
      (wsockEvent<> co ch ssl? msg)
      (httpEvent<> co ch ssl? msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- h1Handler->onRead
  ""
  [^ChannelHandlerContext ctx ^Puglet co ^WholeRequest req]
  (let [{:keys [waitMillis]}
        (.config co)
        ^HttpMsg
        evt (evt<> co {:msg req
                       :ch (.channel ctx)})]
    (if (spos? waitMillis)
      (.hold co evt waitMillis))
    (dispatch! evt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- boot<>
  ""
  [co]
  (let
    [cfg (.config ^Puglet co)
     bs
     (httpServer<>
       (proxy [CPDecorator][]
         (forH1 [_]
           (ihandler<>
             #(h1Handler->onRead %1 co %2)))) cfg)
     ch (startServer bs cfg)]
    [bs ch]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- processOrphan
  ""
  [_]
  ;; 500 or 503
  (workStream<>
    (script<>
      #(let [^Job job %2
             s (or (.getv job :statusCode)
                   500)
             ^HttpMsg evt (.origin job)]
         (->> (httpResult<>
                (.socket evt)
                (.msgGist evt)
                (HttpResponseStatus/valueOf s))
              (replyResult (.socket evt)))
         nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn hookShutdown
  ""
  [^Atom gist func arg]
  (let [ch (-> (discardHTTPD<> func)
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
  {:info {:name "HTTP Server"
          :version "1.0.0"}
   :conf {:maxInMemory (* 1024 1024 4)
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
  [co {:keys [conf] :as spec}]
  (let
    [pkey (.podKey (.server ^Puglet co))
     bee (keyword (juid))
     cee (keyword (juid))
     impl (muble<>)]
    (reify Pluggable
      (spec [_] httpspecdef)
      (init [_ arg]
        (.copyEx impl
                 (httpBasicConfig pkey conf arg)))
      (start [_ _]
        (let [[bs ch] (boot<> co)]
          (.setv impl bee bs)
          (.setv impl cee ch)))
      (stop [_]
        (when-some [c (.getv impl cee)]
          (stopServer c)
          (.unsetv impl bee)
          (.unsetv impl cee)))
      (config [_] (.intern impl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loadTemplate
  ""
  ^APersistentMap
  [cfg tpath data]
  (let
    [ts (str "/" (triml tpath "/"))
     out (ftl/renderFtl cfg ts data)]
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
  {:info {:name "Web Site"
          :version "1.0.0"}
   :conf {:maxInMemory (* 1024 1024 4)
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
  [co {:keys [conf] :as spec}]
  (let
    [pkey (.podKey (.server ^Puglet co))
     bee (keyword (juid))
     cee (keyword (juid))
     impl (muble<>)]
    (reify Pluggable
      (spec [_] mvcspecdef)
      (init [_ arg]
        (.copyEx impl
                 (httpBasicConfig pkey conf arg))
        (.setv impl
               :ftlCfg
               (ftl/genFtlConfig {:root arg})))
      (start [_ _]
        (let [[bs ch] (boot<> co)]
          (.setv impl bee bs)
          (.setv impl cee ch)))
      (stop [_]
        (when-some [c (.getv impl cee)]
          (stopServer c)
          (.unsetv impl bee)
          (.unsetv impl cee)))
      (config [_] (.intern impl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn WebMVC
  ""
  ^Pluggable
  [co spec]
  (httpMVC<> co spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn HTTP
  ""
  ^Pluggable
  [co spec]
  (httpXXX<> co spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


