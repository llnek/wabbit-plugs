;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for JMS service."
      :author "Kenneth Leung" }

  czlab.wabbit.plugs.io.jms

  (:require [czlab.twisty.codec :refer [passwd<>]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.base.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.wabbit.plugs.io.core])

  (:import [java.util Hashtable Properties ResourceBundle]
           [czlab.wabbit.ctl Pluglet Pluggable]
           [javax.jms
            ConnectionFactory
            Connection
            Destination
            Connection
            Message
            MessageConsumer
            MessageListener
            Queue
            QueueConnection
            QueueConnectionFactory
            QueueReceiver
            QueueSession
            Session
            Topic
            TopicConnection
            TopicConnectionFactory
            TopicSession
            TopicSubscriber]
           [java.io IOException]
           [clojure.lang APersistentMap]
           [javax.naming Context InitialContext]
           [czlab.wabbit.plugs.io JmsMsg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  "" [co {:keys [msg]}]

  (let [eeid (str "event#" (seqint2))
        impl (muble<>)]
    (with-meta
      (reify JmsMsg
        (checkAuthenticity [_] false)
        (id [_] eeid)
        (source [_] co)
        (message [_] msg))
      {:typeid ::JmsMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- onMsg "" [co msg]
  ;;if (msg!=null) block { () => msg.acknowledge() }
  (dispatch! (evt<> co {:msg msg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- inizFac
  "" ^Connection
  [^Pluggable co ^InitialContext ctx ^ConnectionFactory cf]

  (let
    [{:keys [^String destination
             ^String jmsPwd
             ^String jmsUser]}
     (.config co)
     ^Pluglet pg (.parent co)
     pwd (->> (.server pg)
              .pkey
              (passwd<> jmsPwd))
     c (.lookup ctx destination)
     ^Connection
     conn (if (hgl? jmsUser)
            (.createConnection
              cf
              jmsUser
              (stror pwd nil))
            (.createConnection cf))]
    (if (inst? Destination c)
      ;;TODO ? ack always ?
      (-> (.createSession conn false Session/CLIENT_ACKNOWLEDGE)
          (.createConsumer c)
          (.setMessageListener
            (reify MessageListener
              (onMessage [_ m] (onMsg pg m)))))
      (throwIOE "Object not of Destination type"))
    conn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- inizTopic
  "" ^Connection
  [^Pluggable co ^InitialContext ctx ^TopicConnectionFactory cf]

  (let
    [{:keys [^String destination
             ^String jmsUser
             durable?
             ^String jmsPwd]}
     (.config co)
     ^Pluglet pg (.parent co)
     pwd (->> (.server pg)
              .pkey
              (passwd<> jmsPwd))
     conn (if (hgl? jmsUser)
            (.createTopicConnection
              cf
              jmsUser
              (stror pwd nil))
            (.createTopicConnection cf))
     s (.createTopicSession
         conn false Session/CLIENT_ACKNOWLEDGE)
     t (.lookup ctx destination)]
    (if-not (inst? Topic t)
      (throwIOE "Object not of Topic type"))
    (-> (if durable?
          (.createDurableSubscriber s t (juid))
          (.createSubscriber s t))
        (.setMessageListener
          (reify MessageListener
            (onMessage [_ m] (onMsg pg m)))))
    conn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- inizQueue
  "" ^Connection
  [^Pluggable co ^InitialContext ctx ^QueueConnectionFactory cf]

  (let
    [{:keys [^String destination
             ^String jmsUser
             ^String jmsPwd]}
     (.config co)
     ^Pluglet pg (.parent co)
     pwd (->> (.server pg)
              .pkey
              (passwd<> jmsPwd))
     conn (if (hgl? jmsUser)
            (.createQueueConnection
              cf
              jmsUser
              (stror pwd nil))
            (.createQueueConnection cf))
     s (.createQueueSession conn
                            false Session/CLIENT_ACKNOWLEDGE)
     q (.lookup ctx destination)]
    (if-not (inst? Queue q)
      (throwIOE "Object not of Queue type"))
    (-> (.createReceiver s ^Queue q)
        (.setMessageListener
          (reify MessageListener
            (onMessage [_ m] (onMsg pg m)))))
    conn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sanitize
  ""
  [pkey {:keys [jndiPwd jmsPwd] :as cfg}]

  (-> (assoc cfg :jndiPwd (.text (passwd<> jndiPwd pkey)))
      (assoc :jmsPwd (.text (passwd<> jmsPwd pkey)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- start2

  ""
  ^Connection
  [^Pluggable co {:keys [contextFactory
                         providerUrl
                         jndiUser jndiPwd connFactory]}]

  (let
    [vars (Hashtable.)]
    (if (hgl? providerUrl)
      (.put vars Context/PROVIDER_URL providerUrl))
    (if (hgl? contextFactory)
      (.put vars
            Context/INITIAL_CONTEXT_FACTORY
            contextFactory))
    (when (hgl? jndiUser)
      (.put vars "jndi.user" jndiUser)
      (if (hgl? jndiPwd)
        (.put vars "jndi.password" jndiPwd)))
    (let
      [ctx (InitialContext. vars)
       obj (->> (str connFactory)
                (.lookup ctx))
       ^Connection
       c (condp instance? obj
           QueueConnectionFactory
           (inizQueue co ctx obj)
           TopicConnectionFactory
           (inizTopic co ctx obj)
           ConnectionFactory
           (inizFac co ctx obj))]
      (if (nil? c)
        (throwIOE "Unsupported JMS Connection Factory"))
      (.start c)
      c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  specdef
  {:info {:name "JMS Client"
          :version "1.0.0"}
   :conf {:$pluggable ::JMS
          :contextFactory "czlab.proto.mock.jms.MockContextFactory"
          :providerUrl "java://aaa"
          :connFactory "tcf"
          :destination "topic.abc"
          :jndiUser "root"
          :jndiPwd "root"
          :jmsUser "anonymous"
          :jmsPwd "anonymous"
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn JMSSpec "" ^APersistentMap [] specdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn JMS "" ^Pluggable

  ([_] (JMS _ (JMSSpec)))
  ([_ {:keys [conf] :as pspec}]
   (let
     [impl (muble<>)]
     (reify
       Pluggable
       (setParent [_ p] (.setv impl :$parent p))
       (parent [_] (.getv impl :$parent))
       (config [_] (dissoc (.intern impl)
                           :$parent :$conn))
       (spec [_] pspec)
       (init [this arg]
         (let [^Pluglet pg (.parent this)
               k (.. pg server pkey)]
           (.copyEx impl
                    (merge conf
                         (sanitize k arg)))))
       (start [this _]
         (when-some [c (start2 this (.intern impl))]
           (.setv impl :$conn c)))
       (stop [_]
         (when-some
           [^Connection c (.unsetv impl :$conn)]
           (try! (.close c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

