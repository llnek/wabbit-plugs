;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.mvc

  (:require [clojure.walk :refer [postwalk]]
            [czlab.basal.logging :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.convoy.nettio.resp]
        [czlab.wabbit.base.core]
        [czlab.convoy.net.core]
        [czlab.convoy.net.wess]
        [czlab.basal.consts]
        [czlab.basal.core]
        [czlab.basal.io]
        [czlab.basal.str]
        [czlab.flux.wflow.core]
        [czlab.wabbit.plugs.io.core])

  (:import [clojure.lang APersistentMap]
           [freemarker.template
            TemplateMethodModelEx
            TemplateBooleanModel
            TemplateCollectionModel
            TemplateDateModel
            TemplateHashModelEx
            TemplateNumberModel
            TemplateScalarModel
            TemplateSequenceModel
            TemplateMethodModel
            Configuration
            DefaultObjectWrapper]
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
           [java.io File Writer StringWriter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol CljApi (->clj [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(extend-protocol CljApi

  TemplateCollectionModel
  (->clj [itr]
    (loop [acc []]
      (if (and (some? itr)
               (.hasNext itr))
        (recur (conj acc
                     (->clj (.next itr))))
        acc)))

  TemplateSequenceModel
  (->clj [s]
    (for [n (range (.size s))]
      (->clj (.get s n))))

  TemplateHashModelEx
  (->clj [m]
    (zipmap (->clj (.keys m))
            (->clj (.values m))))

  TemplateBooleanModel
  (->clj [_] (.getAsBoolean _))

  TemplateNumberModel
  (->clj [_] (.getAsNumber _))

  TemplateScalarModel
  (->clj [_] (.getAsString _))

  TemplateDateModel
  (->clj [_] (.getAsDate _))

  Object
  (->clj [_]
    (throwBadArg
      "Failed to convert %s" (class _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- asFtlMethod "" [func]

  (reify
    TemplateMethodModelEx
    (exec [_ args] (apply func (map ->clj args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- skey "" [[k v]]

  [(cs/replace (strKW k) #"[$!#*+\-]" "_")  v])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- asFtlModel "" [m]

  (postwalk #(cond
               (map? %) (into {} (map skey %))
               (fn? %) (asFtlMethod %)
               :else %) m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn genFtlConfig
  ""
  {:tag Configuration}

  ([] (genFtlConfig nil))
  ([{:keys [root shared] :or {shared {}}}]
   (let [cfg (Configuration.)]
     (if-some [dir (io/file root)]
       (when (.exists dir)
         (log/info "freemarker template source: %s" root)
         (doto cfg
           (.setDirectoryForTemplateLoading dir)
           (.setObjectWrapper (DefaultObjectWrapper.)))
         (doseq [[k v] (asFtlModel shared)]
           (.setSharedVariable cfg ^String k v))))
     cfg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn renderFtl

  "Renders a template given by Configuration cfg and a path
   using model as input and writes it to out
   If out is not provided, returns a string
   If xrefModel? is true, asFtlModel is run on the model"
  {:tag String}

  ([cfg writer path model]
   (renderFtl cfg writer path model nil))

  ([cfg path model]
   (renderFtl cfg (StringWriter.) path model nil))

  ([^Configuration cfg ^Writer out
    ^String path model {:keys [xrefModel?]
                        :or {xrefModel? true}}]
   (some-> (.getTemplate cfg path)
           (.process (if xrefModel?
                       (asFtlModel model) model) out))
   (str out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn loadTemplate
  "" ^APersistentMap [^Pluglet co tpath data]

  (let
    [c (. (.getx co) getv :$ftlCfg)
     ts (str "/" (triml tpath "/"))
     out (renderFtl c ts data)]
    {:data (xdata<> out)
     :ctype
     (cond
       (.endsWith ts ".json") "application/json"
       (.endsWith ts ".xml") "application/xml"
       (.endsWith ts ".html") "text/html"
       :else "text/plain")}))

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
  (let
    [^Channel ch (.socket evt)
     gist (.msgGist evt)
     res (httpResult<> ch gist)
     fp (io/file file)]
    (log/debug "serving file: %s" (fpath fp))
    (try
      (if (or (nil? fp)
              (not (.exists fp)))
        (do
          (.setStatus res 404)
          (replyResult res))
        (do
          (.setContent res fp)
          (replyResult res)))
      (catch Throwable e#
        (log/error e# "get: %s" (:uri gist))
        (try!
          (.setStatus res 500)
          (.setContent res nil)
          (replyResult res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private asset-handler
  (workStream<>
    (script<>
      #(let
         [^HttpMsg evt (.origin ^Job %2)
          cfg (.. evt source config)
          homeDir (.. evt
                      source
                      server homeDir)
          r (.routeGist evt)
          mp (some-> ^RouteInfo
                     (:info r) .mountPoint)
          fp (str
               (reduce
                 (fn [a b] (cs/replace-first a "{}" b))
                 (cs/replace (str mp)
                             "${pod.dir}" (fpath homeDir))
                 (:groups r)))
          pubDir (io/file homeDir dn-pub)
          check? (:fileAccessCheck? cfg)
          gist (.msgGist evt)]
         (log/debug "request for asset: %s" fp)
         (if (or (false? check?)
                 (.startsWith fp
                              (fpath pubDir)))
           (->> (maybeStripUrlCrap fp)
                (getStatic evt))
           (let [ch (.socket evt)]
             (log/warn "illegal access: %s" fpath)
             (-> (httpResult<> ch gist 403)
                 replyResult )))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn asset! "" ^WorkStream [] asset-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

