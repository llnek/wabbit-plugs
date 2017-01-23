;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.jmx.core

  (:require [czlab.basal.logging :as log]
            [clojure.string :as cs])

  (:use [czlab.wabbit.plugs.jmx.bean]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.net InetAddress MalformedURLException]
           [java.rmi.registry LocateRegistry Registry]
           [czlab.wabbit.jmx JmxPluglet]
           [czlab.wabbit.ctl Pluggable]
           [java.lang.management ManagementFactory]
           [java.rmi.server UnicastRemoteObject]
           [java.rmi NoSuchObjectException]
           [czlab.wabbit.sys Execvisor]
           [czlab.jasal Muble]
           [java.util HashMap]
           [javax.management.remote
            JMXConnectorServer
            JMXServiceURL
            JMXConnectorServerFactory]
           [javax.management
            JMException
            MBeanServer
            ObjectName
            DynamicMBean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn objectName<>
  "paths: [ \"a=b\" \"c=d\" ]
   domain: com.acme
   beanName: mybean"
  {:tag ObjectName}
  ([domain beanName] (objectName<> domain beanName nil))
  ([^String domain ^String beanName paths]
   (let [cs (seq (or paths []))
         sb (strbf<>)]
     (doto sb
       (.append domain)
       (.append ":")
       (.append (cs/join "," cs)))
     (if-not (empty? cs) (.append sb ","))
     (doto sb
       (.append "name=")
       (.append beanName))
     (ObjectName. (.toString sb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkJMXrror
  ""
  [^String msg ^Throwable e]
  (throw (doto (JMException. msg) (.initCause e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- startRMI
  ""
  [^Muble impl]
  (try
    (->> (long (.getv impl :registryPort))
         (LocateRegistry/createRegistry )
         (.setv impl :rmi ))
    (catch Throwable _
      (mkJMXrror "Failed to create RMI registry" _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- startJMX
  ""
  [^Muble impl]
  (let
    [cfc "com.sun.jndi.rmi.registry.RegistryContextFactory"
     svc (str "service:jmx:rmi://{{h}}:{{s}}"
              "/jndi/rmi://{{h}}:{{r}}/jmxrmi")
     {:keys [registryPort serverPort
             host url contextFactory]}
     (.intern impl)
     env (HashMap.)
     endpt (-> (cs/replace (stror url svc) "{{h}}" host)
               (cs/replace "{{s}}" (str serverPort))
               (cs/replace "{{r}}" (str registryPort)))]
    (log/debug "jmx service url: %s" endpt)
    (.put env
          "java.naming.factory.initial"
          (stror contextFactory cfc))
    (let
      [conn (JMXConnectorServerFactory/newJMXConnectorServer
              (JMXServiceURL. endpt)
              env
              (ManagementFactory/getPlatformMBeanServer))]
      (.start conn)
      (doto impl
        (.setv :beanSvr (.getMBeanServer conn))
        (.setv :conn conn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doReg
  ""
  [^MBeanServer svr ^ObjectName objName ^DynamicMBean mbean]
  (.registerMBean svr mbean objName)
  (log/info "jmx-bean: %s" objName)
  objName)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- jmsPluglet<>
  ""
  ^JmxPluglet
  [^Execvisor ctr]
  (let
    [impl (muble<> {:registryPort 7777
                    :serverPort 7778
                    :host (-> (InetAddress/getLocalHost)
                              (.getHostName))})
     pid (str "jmx#" (seqint2))
     objNames (atom [])]
    (reify JmxPluglet

      (isEnabled [this]
        (not (false? (:enabled? (.config this)))))

      (config [_] (.intern impl))
      (spec [_] nil)

      (server [_] ctr)
      (hold [_ _ _])
      (version [_] "")
      (id [_] pid)
      (getx [_] impl)

      (reset [this]
        (let [bs (.getv impl :beanSvr)]
          (doseq [nm @objNames]
            (try!
              (.dereg this nm)))
          (reset! objNames [])))

      (dereg [_ objName]
        (let [bs (.getv impl :beanSvr)]
          (-> ^MBeanServer bs
              (.unregisterMBean objName))))

      (reg [_ obj domain nname paths]
        (let [nm (objectName<> domain nname paths)
              bs (.getv impl :beanSvr)]
          (->> (doReg bs nm (jmxBean<> obj))
               (swap! objNames conj))
          nm))

      (init [_ arg]
        (->> (or arg {})
             (.copyEx impl )))

      (start [_ _]
        (startRMI impl)
        (startJMX impl)
        (log/info "JmxPluglet started"))

      (stop [this]
        (let [^JMXConnectorServer c (.getv impl :conn)
              ^Registry r (.getv impl :rmi)]
          (.reset this)
          (try! (some-> c (.stop )))
          (.unsetv impl :conn)
          (try!
            (some-> r
                    (UnicastRemoteObject/unexportObject  true)))
          (.unsetv impl :rmi)
          (log/info "JmxPluglet stopped")))

      (dispose [_]
        (log/info "JmxPluglet disposed")))))

  ;; jconsole port
  ;;(setRegistryPort [_ p] (.setv impl :registryPort p))
  ;;(setServerPort[_ p] (.setv impl :serverPort p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn JmxMonitor "" ^JmxPluglet [ctr] (jmsPluglet<> ctr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

