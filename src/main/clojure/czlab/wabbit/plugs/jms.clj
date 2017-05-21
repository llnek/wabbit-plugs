;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for JMS service."
      :author "Kenneth Leung" }

  czlab.wabbit.plugs.jms

  (:require [czlab.basal.log :as log]
            [czlab.twisty.codec :as co]
            [czlab.wabbit.base :as b]
            [czlab.wabbit.xpis :as xp]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [czlab.basal.str :as s]
            [czlab.wabbit.plugs.core :as pc])

  (:import [java.util Hashtable Properties ResourceBundle]
           [czlab.jasal Hierarchical LifeCycle Idable]
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
           [javax.naming Context InitialContext]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(c/decl-object JmsMsg
               xp/PlugletMsg
               (get-pluglet [me] (:$source me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private evt<>
  "" [co msg]

  `(c/object<> JmsMsg
               {:id (str "JmsMsg." (c/seqint2)) :$source ~co :message ~msg}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private onMsg "" [co msg]
  ;;if (msg!=null) block { () => msg.acknowledge() }
  `(pc/dispatch! (evt<> ~co ~msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- inizFac
  "" ^Connection
  [co ^InitialContext ctx ^ConnectionFactory cf]

  (let
    [{:keys [destination jmsPwd jmsUser]}
     (:conf @co)
     pwd (->> co
              xp/get-server
              xp/pkey-chars (co/pwd<> jmsPwd) co/p-text)
     c (.lookup ctx ^String destination)]
    (c/do-with
      [^Connection
       conn (if (s/hgl? jmsUser)
              (.createConnection
                cf
                ^String jmsUser
                (s/stror (c/strit pwd) nil))
              (.createConnection cf))]
      (if (c/ist? Destination c)
        ;;TODO ? ack always ?
        (-> (.createSession conn false Session/CLIENT_ACKNOWLEDGE)
            (.createConsumer c)
            (.setMessageListener
              (reify MessageListener
                (onMessage [_ m] (onMsg co m)))))
        (c/throwIOE "Object not of Destination type")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- inizTopic
  "" ^Connection
  [co ^InitialContext ctx ^TopicConnectionFactory cf]

  (let
    [{:keys [destination jmsUser durable? jmsPwd]}
     (:conf @co)
     pwd (->> co
              xp/get-server
              xp/pkey-chars (co/pwd<> jmsPwd) co/p-text)]
    (c/do-with
      [conn (if (s/hgl? jmsUser)
              (.createTopicConnection
                cf
                ^String jmsUser
                (s/stror (c/strit pwd) nil))
              (.createTopicConnection cf))]
      (let
        [s (.createTopicSession
             conn false Session/CLIENT_ACKNOWLEDGE)
         t (.lookup ctx ^String destination)]
        (if-not (c/ist? Topic t)
          (c/throwIOE "Object not of Topic type"))
        (-> (if durable?
              (.createDurableSubscriber s t (c/jid<>))
              (.createSubscriber s t))
            (.setMessageListener
              (reify MessageListener
                (onMessage [_ m] (onMsg co m)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- inizQueue
  "" ^Connection
  [co ^InitialContext ctx ^QueueConnectionFactory cf]

  (let
    [{:keys [destination jmsUser jmsPwd]}
     (:conf @co)
     pwd (->> co
              xp/get-server
              xp/pkey-chars (co/pwd<> jmsPwd) co/p-text)]
    (c/do-with
      [conn (if (s/hgl? jmsUser)
              (.createQueueConnection
                cf
                ^String jmsUser
                (s/stror (c/strit pwd) nil))
              (.createQueueConnection cf))]
      (let
        [s (.createQueueSession conn
                                false Session/CLIENT_ACKNOWLEDGE)
         q (.lookup ctx ^String destination)]
        (if-not (c/ist? Queue q)
          (c/throwIOE "Object not of Queue type"))
        (-> (.createReceiver s ^Queue q)
            (.setMessageListener
              (reify MessageListener
                (onMessage [_ m] (onMsg co m)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sanitize
  ""
  [pkey {:keys [jndiPwd jmsPwd] :as cfg}]

  (-> (assoc cfg :jndiPwd (co/p-text (co/pwd<> jndiPwd pkey)))
      (assoc :jmsPwd (co/p-text (co/pwd<> jmsPwd pkey)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- start2

  ""
  ^Connection
  [co {:keys [contextFactory
              providerUrl
              jndiUser jndiPwd connFactory]}]
  (let
    [vars (Hashtable.)]
    (if (s/hgl? providerUrl)
      (.put vars Context/PROVIDER_URL providerUrl))
    (if (s/hgl? contextFactory)
      (.put vars
            Context/INITIAL_CONTEXT_FACTORY
            contextFactory))
    (when (s/hgl? jndiUser)
      (.put vars "jndi.user" jndiUser)
      (if (s/hgl? jndiPwd)
        (.put vars "jndi.password" jndiPwd)))
    (let
      [ctx (InitialContext. vars)
       obj (->> (str connFactory)
                (.lookup ctx))]
      (c/do-with
        [^Connection
         c (condp instance? obj
             QueueConnectionFactory
             (inizQueue co ctx obj)
             TopicConnectionFactory
             (inizTopic co ctx obj)
             ConnectionFactory
             (inizFac co ctx obj))]
        (if (nil? c)
          (c/throwIOE "Unsupported JMS Connection Factory"))
        (.start c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable JmsPluglet
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me arg]
    (let [k (-> me xp/get-server xp/pkey-chars)
          c (get-in @me [:pspec :conf])]
      (->> (b/prevarCfg (sanitize k
                                  (merge c arg)))
           (c/setf! me :conf))))
  (start [me] (.start me nil))
  (start [me arg]
    (->> (:conf @me) (start2 me) (c/setf! me :$conn)))
  (stop [me]
    (when-some [c (:$conn @me)]
      (c/try! (i/closeQ c))))
  (dispose [me] (.stop me)))

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
(defn JMS ""
  ([_ id] (JMS _ id (JMSSpec)))
  ([_ id spec]
   (c/mutable<> JmsPluglet
                {:pspec (update-in spec
                                   [:conf] b/expandVarsInForm)
                 :parent _
                 :emAlias id })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

