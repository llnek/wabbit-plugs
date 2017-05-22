;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for HTTP/MVC service."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.http

  (:require [czlab.convoy.util :as ct :refer [parseBasicAuth]]
            [czlab.basal.format :as f :refer [readEdn]]
            [czlab.wabbit.plugs.mvc :as mvc]
            [czlab.wabbit.plugs.core :as pc]
            [czlab.nettio.discard :as ds]
            [czlab.convoy.routes :as cr]
            [czlab.nettio.server :as sv]
            [czlab.twisty.codec :as co]
            [czlab.nettio.core :as nc]
            [czlab.wabbit.base :as b]
            [czlab.wabbit.xpis :as xp]
            [czlab.convoy.core :as cc]
            [czlab.convoy.wess :as ss]
            [czlab.twisty.ssl :as ssl]
            [czlab.basal.log :as log]
            [clojure.java.io :as io]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [clojure.string :as cs]
            [czlab.basal.str :as s]
            [czlab.basal.meta :as m])

  (:import [czlab.nettio.core NettyWsockMsg NettyH1Msg]
           [java.nio.channels ClosedChannelException]
           [io.netty.handler.codec DecoderException]
           [clojure.lang IDeref Atom APersistentMap]
           [io.netty.handler.codec.http.websocketx
            TextWebSocketFrame
            WebSocketFrame
            BinaryWebSocketFrame]
           [io.netty.handler.codec.http.cookie
            ServerCookieDecoder
            ServerCookieEncoder]
           [io.netty.bootstrap ServerBootstrap]
           [io.netty.buffer ByteBuf Unpooled]
           [io.netty.handler.ssl SslHandler]
           [czlab.nettio InboundHandler]
           [czlab.jasal
            LifeCycle
            Startable
            Idable
            Hierarchical]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(extend-protocol xp/PlugletMsg
  NettyWsockMsg
  (get-pluglet [me] (:$source me))
  NettyH1Msg
  (get-pluglet [me] (:$source me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn scanBasicAuth
  "Scan and parse if exists basic authentication"
  ^APersistentMap
  [evt]
  (c/if-some+
    [v (cc/msgHeader evt auth-token)] (ct/parseBasicAuth v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeLoadRoutes
  "" [{:keys [routes]}]
  (when-not (empty? routes) (cr/loadRoutes routes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resumeOnExpiry "" [evt]
  (try
    (nc/replyStatus (:socket evt) 500)
    (catch ClosedChannelException _
      (log/warn "channel closed already"))
    (catch Throwable t# (log/exception t#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wsockEvent<> "" [co ch msg]
  (merge msg
         {:id (str "WsockMsg." (c/seqint2))
          :socket ch
          :$source co
          :ssl? (nc/maybeSSL? ch) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpEvent<> "" [co ch req]
  (let
    [{:keys [wantSession? session]}
     (:conf @co)
     {:keys [route cookies]} req]
    (merge req
           {:session (if (and (c/!false? wantSession?)
                              (:session? (:info route)))
                       (ss/upstream (-> co
                                        xp/get-server
                                        xp/pkey-bytes)
                                    cookies
                                    (:macit? session)))
            :$source co
            :stale? false
            :id (str "HttpMsg." (c/seqint2))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  "" [co ch msg]
  (cond
    (satisfies? cc/WsockMsgGist msg)
    (wsockEvent<> co ch msg)
    (satisfies? cc/HttpMsgGist msg)
    (httpEvent<> co ch msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- funky "" [evt]
  (if
    (satisfies? cc/HttpMsgGist evt)
    (let [res (cc/http-result evt)]
      (fn [h e] (h e res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- boot! "" ^LifeCycle [co]
  (let
    [asset! #'czlab.wabbit.plugs.mvc/assetLoader
     {:keys [waitMillis] :as cfg}
     (:conf @co)]
    (log/debug "boot! http-plug: %s" cfg)
    (c/do-with
      [w
       (-> {:hh1
            (fn [ctx msg]
              (let [ev (evt<> co (nc/ch?? ctx) msg)
                    {:keys [static? handler]
                     :as rcfg}
                    (get-in msg [:route :info])
                    _ (log/debug "route info= %s" rcfg)
                    hd (if (and static?
                                (nil? handler)) asset! handler)]
                ;;(if (spos? waitMillis) (hold-event co ev waitMillis))
                (pc/dispatch! ev
                              {:handler hd
                               :dispfn (funky ev)})))}
           (merge cfg)
           sv/nettyWebServer<>)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn processOrphan
  "" [evt error]
  ;; 500 or 503
  (let [ch (:socket evt)] (nc/replyStatus ch 500)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn Discarder! "" [func arg]
  (let
    [^LifeCycle
     w (ds/discardHTTPD<> func arg)] (.start w arg) #(.stop w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- basicfg

  "Basic http config"
  [pkey conf cfg0]

  (let [{:keys [serverKey
                port
                passwd] :as cfg}
        (merge conf cfg0)
        ssl? (s/hgl? serverKey)]
    (if ssl?
      (c/test-cond "server-key file url"
                   (. ^String serverKey startsWith "file:")))
    (merge cfg
           {:port (if-not (c/spos? port) (if ssl? 443 80) port)
            :routes (maybeLoadRoutes cfg)
            :passwd (co/p-text (co/pwd<> passwd pkey))
            :serverKey (if ssl? (io/as-url serverKey))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable HttpPluglet
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me arg]
    (let [k (-> me xp/get-server xp/pkey-chars)
          c (get-in @me [:pspec :conf])
          cfg (b/prevarCfg (basicfg k c arg))
          {:keys [publicRootDir pageDir]}
          (:wsite cfg)
          pub (io/file (str publicRootDir)
                       (str pageDir))]
      (when (i/dirReadWrite? pub)
        (log/debug "freemarker tpl root: %s" (c/fpath pub))
        (c/setf! me :ftlCfg (mvc/genFtlConfig {:root pub})))
      (c/setf! me :conf cfg)))
  (start [me] (.start me nil))
  (start [me arg]
    (let [w (boot! me)]
      (c/setf! me :boot w)
      (.start w (:conf @me))))
  (dispose [me] (.stop me))
  (stop [me]
    (let [{:keys [boot]} @me]
      (some-> ^Startable boot .stop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  httpspecdef
  {:deps {:$auth [:czlab.wabbit.shiro.core/WebAuth]}
   :error :czlab.wabbit.plugs.http/processOrphan
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
(defn HTTP ""
  ([_ id] (HTTP _ id (HTTPSpec)))
  ([_ id spec]
   (c/mutable<> HttpPluglet
                {:pspec (update-in spec
                                   [:conf] b/expandVarsInForm)
                 :parent _
                 :emAlias id
                 :timer (Timer. true)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

