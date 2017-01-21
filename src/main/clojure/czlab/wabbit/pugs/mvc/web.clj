;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.pugs.mvc.web

  (:require [czlab.twisty.core :refer [genMac]]
            [czlab.basal.io :refer [hexify]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [czlab.wabbit.base.core]
        [czlab.convoy.net.core]
        [czlab.wabbit.io.http]
        [czlab.basal.consts]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.flux.wflow.core]
        [czlab.wabbit.io.core])

  (:import [czlab.jasal CU XData Muble Hierarchial Identifiable]
           [czlab.wabbit.pugs.auth ExpiredError AuthError]
           [czlab.wabbit.ctl Puglet PugEvent]
           [czlab.wabbit.pugs.io HttpEvent]
           [czlab.flux.wflow WorkStream Job]
           [czlab.wabbit.sys Container]
           [java.net HttpCookie]
           [czlab.convoy.net
            WebContent
            HttpResult
            RouteInfo
            HttpSession
            RouteCracker]
           [java.util Date]
           [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private ^String session-cookie "__ss117")
(def ^:private ssid-flag :__f01es)
(def ^:private cs-flag :__f184n ) ;; creation time
(def ^:private ls-flag :__f384n ) ;; last access time
(def ^:private es-flag :__f484n ) ;; expiry time
(def ^:private ^String nv-sep "\u0000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resetFlags
  "A negative value means that the cookie is not stored persistently and will be deleted when the Web browser exits. A zero value causes the cookie to be deleted."
  [^HttpSession mvs maxAgeSecs]
  (let [maxAgeSecs (or maxAgeSecs -1)
        now (now<>)]
    (doto mvs
      (.setAttr ssid-flag
                (hexify (bytesify (juid))))
      (.setAttr es-flag
                (if (spos? maxAgeSecs)
                  (+ now (* maxAgeSecs 1000))
                  maxAgeSecs))
      (.setAttr cs-flag now)
      (.setAttr ls-flag now))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn wsession<>
  ""
  ^HttpSession
  [^bytes pkey ssl?]
  (let [impl (muble<> {:maxIdleSecs 0
                       :newOne true})
        _attrs (muble<>)]
    (with-meta
      (reify HttpSession

        (removeAttr [_ k] (.unsetv _attrs k))
        (setAttr [_ k v] (.setv _attrs k v))
        (attr [_ k] (.getv _attrs k))
        (clear [_] (.clear _attrs))
        (attrs [_] (.seq _attrs))

        (setMaxIdleSecs [_ idleSecs]
          (if (number? idleSecs)
            (.setv impl :maxIdleSecs idleSecs)))

        (isNull [_] (empty? (.intern impl)))
        (isNew [_] (.getv impl :newOne))
        (isSSL [_] ssl?)

        (invalidate [_]
          (.clear _attrs)
          (.clear impl))

        (signer [_] pkey)

        (setXref [_ csrf]
          (.setv _attrs :csrf csrf))

        (setNew [this flag maxAge]
          (when flag
            (.clear _attrs)
            (.clear impl)
            (resetFlags this maxAge)
            (.setv impl :maxIdleSecs 0))
          (.setv impl :newOne flag))

        (maxIdleSecs [_] (or (.getv impl :maxIdleSecs) 0))
        (creationTime [_] (or (.getv _attrs cs-flag) 0))
        (expiryTime [_] (or (.getv _attrs es-flag) 0))
        (xref [_] (.getv _attrs :csrf))
        (id [_] (.getv _attrs ssid-flag))

        (lastError [_] (.getv impl :error))

        (lastAccessedTime [_]
          (or (.getv _attrs ls-flag) 0))

        Object

        (toString [this]
          (str
            (reduce
              #(let [[k v] %2]
                 (addDelim! %1
                            nv-sep
                            (str (name k) ":" v)))
              (strbf<>)
              (.seq _attrs)))))

        {:typeid ::HttpSession })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeMacIt
  ""
  [gist pkey ^String data]
  (if
    (boolean
      (some-> ^RouteInfo
              (get-in gist [:route :info])
              (.isSecure)))
    (str (genMac pkey data) "-" data)
    data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn downstream
  ""
  [^HttpEvent evt ^HttpResult res]
  (let
    [{:keys [sessionAgeSecs
             domainPath
             domain
             hidden
             maxIdleSecs]}
     (.. evt source config)
     mvs (.session evt)
     gist (.msgGist evt)]
    (when-not (.isNull mvs)
      (log/debug "session ok, about to set-cookie!")
      (if (.isNew mvs)
        (->> (or sessionAgeSecs 3600)
             (resetFlags mvs )))
      (let
        [ck (->> (maybeMacIt gist
                             (.signer mvs)
                             (str mvs))
                 (HttpCookie. session-cookie))
         est (.expiryTime mvs)]
        (->> (long (or maxIdleSecs -1))
             (.setMaxIdleSecs mvs))
        (doto ck
          (.setMaxAge (if (spos? est)
                        (/ (- est (now<>)) 1000) 0))
          (.setDomain (str domain))
          (.setSecure (.isSSL mvs))
          (.setHttpOnly (true? hidden))
          (.setPath (str domainPath)))
        (.addCookie res ck)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- testCookie
  ""
  [^bytes pkey ^RouteInfo ri p1 p2]
  (if (boolean (some-> ri (.isSecure)))
    (when (not= (genMac pkey p2) p1)
      (log/error "session cookie - broken")
      (trap! AuthError "Bad Session Cookie")))
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn upstream
  ""
  ^HttpSession
  [gist pkey maxIdleSecs]
  (let [^RouteInfo
        ri (get-in gist [:route :info])
        [^HttpSession mvs
         ^HttpCookie ck]
        (if (.wantSession ri)
          [(wsession<> pkey (:ssl? gist))
           (-> (:cookies gist)
               (get session-cookie))])]
    (cond
      (some? ck)
      (let
        [cookie (str (.getValue ck))
         pos (.indexOf cookie (int \-))
         [^String p1 ^String p2]
         (if (< pos 0)
           ["" cookie]
           [(.substring cookie 0 pos)
            (.substring cookie (inc pos))])
         ok (testCookie pkey ri p1 p2)]
        (log/debug "session attrs= %s" p2)
        (try
          (doseq [^String nv (.split p2 nv-sep)
                  :let [ss (.split nv ":" 2)
                        ^String s1 (aget ss 0)
                        k1 (keyword s1)
                        ^String s2 (aget ss 1)]]
            (log/debug "s-attr n=%s, v=%s" s1 s2)
            (if (and (.startsWith s1 "__f")
                     (.endsWith s1 "n"))
              (->> (convLong s2 0)
                   (.setAttr mvs k1))
              (.setAttr mvs k1 s2)))
          (catch Throwable _
            (trap! ExpiredError "malformed cookie")))
        (.setNew mvs false 0)
        (let [ts (or (.attr mvs ls-flag) -1)
              es (or (.attr mvs es-flag) -1)
              mi (or maxIdleSecs 0)
              now (now<>)]
          (if (or (< es now)
                  (and (spos? mi)
                       (< (+ ts (* 1000 mi)) now)))
            (trap! ExpiredError "Session has expired"))
          (.setAttr mvs ls-flag now)
          mvs))

      (some? mvs)
      (do
        (log/warn "no s-cookie found, invalidate!")
        (.invalidate mvs)))
    mvs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeStripUrlCrap
  "Want to handle case where the url has stuff after the file name.
   For example:  /public/blab&hhh or /public/blah?ggg"
  ^String
  [^String path]

  (let [pos (.lastIndexOf path (int \/))]
    (if (spos? pos)
      (let [p1 (.indexOf path (int \?) pos)
            p2 (.indexOf path (int \&) pos)
            p3 (cond
                 (and (> p1 0)
                      (> p2 0))
                 (Math/min p1 p2)
                 (> p1 0) p1
                 (> p2 0) p2
                 :else -1)]
        (if (> p3 0)
          (.substring path 0 p3)
          path))
      path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getStatic

  ""
  [^HttpEvent evt file]
  (let [^Channel ch (.socket evt)
        res (httpResult<> ch (.msgGist evt))
        gist (.msgGist evt)
        fp (io/file file)]
    (log/debug "serving file: %s" (fpath fp))
    (try
      (if (or (nil? fp)
              (not (.exists fp)))
        (do
          (.setStatus res 404)
          (replyResult ch res))
        (do
          (.setContent res fp)
          (replyResult ch res)))
      (catch Throwable e#
        (log/error "get: %s" (:uri gist) e#)
        (try!
          (.setStatus res 500)
          (.setContent res nil)
          (replyResult ch res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleStatic
  "Handle static resource"
  [^HttpEvent evt args]
  (let
    [podDir (.. evt source server podDir)
     pubDir (io/file podDir dn-pub)
     cfg (.. evt source config)
     check? (:fileAccessCheck? cfg)
     fpath (str (:path args))
     gist (.msgGist evt)]
    (log/debug "request for file: %s" fpath)
    (if (or (.startsWith fpath (fpath pubDir))
            (false? check?))
      (->> (maybeStripUrlCrap fpath)
           (getStatic evt))
      (let [ch (.socket evt)]
        (log/warn "illegal access: %s" fpath)
        (->> (httpResult<> ch (.msgGist evt) 403)
             (replyResult ch))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn serveError
  "Reply back an error"
  [^HttpEvent evt status]
  (try
    (let
      [rts (.. evt source server cljrt)
       res (httpResult<> (.socket evt) (.msgGist evt) status)
       {:keys [errorHandler]}
       (.. evt source config)
       ^WebContent
       rc (if (hgl? errorHandler)
            (.callEx rts
                     errorHandler
                     (.status res)))
       ctype (or (some-> rc (.contentType))
                 "application/octet-stream")
       body (some-> rc (.content))]
      (when (and (some? body)
                 (.hasContent body))
        (.setContentType res ctype)
        (.setContent res body))
      (replyResult (.socket evt) res))
    (catch Throwable _ )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- serveStatic2

  "Reply back with a static file content"
  [^HttpEvent evt]

  (let
    [podDir (.. evt source server podDir)
     pubDir (io/file podDir dn-pub)
     gist (.msgGist evt)
     r (:route gist)
     ^RouteInfo ri (:info r)
     mpt (-> (.getx ri)
             (.getv :mountPoint))
     {:keys [waitMillis]}
     (.. evt source config)
     mpt (reduce
           #(cs/replace-first %1 "{}" %2)
           (.replace (str mpt)
                     "${pod.dir}" (fpath podDir))
           (:groups r))
     mDir (io/file mpt)]
    (if (spos? waitMillis)
      (.hold (.source evt) evt waitMillis))
    (dispatch! evt
               {:router "czlab.wabbit.mvc.web/assetHandler<>"
                :path (fpath mDir)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn serveStatic
  "Reply back with a static file content"
  [^HttpEvent evt]
  (let
    [exp
     (try
       (do->nil
         (upstream (.msgGist evt)
                   (.podKeyBits (.. evt source server))
                   (:maxIdleSecs (.. evt source config))))
       (catch AuthError _ _))]
    (if (some? exp)
      (serveError evt 403)
      (serveStatic2 evt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- serveRoute2
  "Handle a matched route"
  [^HttpEvent evt]
  (let
    [gist (.msgGist evt)
     r (:route gist)
     ^RouteInfo ri (:info r)
     pms (:places r)
     {:keys [waitMillis]}
     (.. evt source config)
     options {:router (.handler ri)
              :params (or pms {})
              :template (.template ri)}]
    (if (spos? waitMillis)
      (.hold (.source evt) evt waitMillis))
    (dispatch! evt options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn serveRoute
  "Handle a matched route"
  [^HttpEvent evt]
  (let
    [exp
     (try
       (do->nil
         (upstream evt
                   (.. evt source server podKeyBits)
                   (:maxIdleSecs (.. evt source config))))
       (catch AuthError _ _))]
    (if (some? exp)
      (serveError evt 403)
      (serveRoute2 evt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private asset-handler
  (workStream<>
    (script<>
      #(let [evt (.event ^Job %2)]
         (handleStatic evt
                       (.getv ^Job %2 evt-opts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn assetHandler<> "" ^WorkStream [] asset-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replyEvent
  ""
  [^HttpEvent evt ^HttpResult res]
  (let [mvs (.session evt)
        code (.status res)]
    (.cancel evt)
    (if (.isStale evt)
      (throwIOE "Event has expired"))
    (if (and (.checkSession evt)
             (or (nil? mvs)
                 (nil? (.isNull mvs))))
      (throwIOE "Invalid/Null session"))
    (if (some? mvs)
      (downstream evt res))
    (replyResult (.socket evt) res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


