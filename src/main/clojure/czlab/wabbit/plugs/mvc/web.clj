;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.mvc.web

  (:require [czlab.twisty.core :refer [genMac]]
            [czlab.basal.io :refer [hexify]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io]
            [clojure.string :as cs])

  (:use [czlab.wabbit.base.core]
        [czlab.convoy.net.core]
        [czlab.convoy.net.wess]
        [czlab.wabbit.plugs.io.http]
        [czlab.basal.consts]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.flux.wflow.core]
        [czlab.wabbit.plugs.io.core])

  (:import [czlab.jasal CU XData Muble Hierarchial Identifiable]
           [czlab.wabbit.ctl Pluglet PlugMsg]
           [czlab.wabbit.plugs.io HttpMsg]
           [czlab.flux.wflow WorkStream Job]
           [java.net HttpCookie]
           [czlab.convoy.net
            WebContent
            HttpResult
            AuthError
            RouteInfo
            HttpSession
            RouteCracker]
           [java.util Date]
           [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

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
  [^HttpMsg evt file]
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
  [^HttpMsg evt args]
  (let
    [homeDir (.. evt source server homeDir)
     pubDir (io/file homeDir dn-pub)
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
  [^HttpMsg evt status]
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
  [^HttpMsg evt]

  (let
    [homeDir (.. evt source server homeDir)
     pubDir (io/file homeDir dn-pub)
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
                     "${pod.dir}" (fpath homeDir))
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
  [^HttpMsg evt]
  (let
    [exp
     (try
       (do->nil
         (upstream (.msgGist evt)
                   (.pkeyBytes (.. evt source server))
                   (:maxIdleSecs (.. evt source config))))
       (catch AuthError _ _))]
    (if (some? exp)
      (serveError evt 403)
      (serveStatic2 evt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- serveRoute2
  "Handle a matched route"
  [^HttpMsg evt]
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
  [^HttpMsg evt]
  (let
    [exp
     (try
       (do->nil
         (upstream evt
                   (.. evt source server pkeyBytes)
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
      #(let [evt (.origin ^Job %2)]
         (handleStatic evt
                       (.getv ^Job %2 evt-opts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn assetHandler<> "" ^WorkStream [] asset-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn replyEvent
  ""
  [^HttpMsg evt ^HttpResult res]
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


