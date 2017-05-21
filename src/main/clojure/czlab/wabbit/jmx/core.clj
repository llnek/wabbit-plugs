;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.jmx.core

  (:require [czlab.wabbit.jmx.bean :as bn]
            [czlab.basal.log :as log]
            [clojure.string :as cs]
            [czlab.wabbit.base :as b]
            [czlab.wabbit.xpis :as xp]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:import [java.net InetAddress MalformedURLException]
           [java.rmi.registry LocateRegistry Registry]
           [java.lang.management ManagementFactory]
           [java.rmi.server UnicastRemoteObject]
           [java.rmi NoSuchObjectException]
           [czlab.jasal
            Resetable
            LifeCycle
            Idable
            Hierarchical]
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

  ([domain beanName]
   (objectName<> domain beanName nil))

  ([^String domain ^String beanName paths]
   (let [cs (seq (or paths []))
         sb (s/strbf<>)]
     (doto sb
       (.append domain)
       (.append ":")
       (.append (cs/join "," cs)))
     (if-not (empty? cs) (.append sb ","))
     (doto sb
       (.append "name=")
       (.append beanName))
     (ObjectName. (str sb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkJMXrror
  "" [^String msg ^Throwable e]
  (throw (doto (JMException. msg) (.initCause e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- startRMI "" [plug]
  (try
    (->> ^long (get-in @plug [:conf :registryPort])
         LocateRegistry/createRegistry (c/setf! plug :rmi))
    (catch Throwable _
      (mkJMXrror "Failed to create RMI registry" _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- startJMX "" [plug]
  (let
    [cfc "com.sun.jndi.rmi.registry.RegistryContextFactory"
     svc (str "service:jmx:rmi://{{h}}:{{s}}"
              "/jndi/rmi://{{h}}:{{r}}/jmxrmi")
     {:keys [registryPort serverPort
             host url contextFactory]}
     (:conf @plug)
     host (->> (-> (InetAddress/getLocalHost) .getHostName)
               (s/stror host ))
     env (HashMap.)
     endpt (-> (cs/replace (s/stror url svc) "{{h}}" host)
               (cs/replace "{{s}}" (str serverPort))
               (cs/replace "{{r}}" (str registryPort)))]
    (log/debug "jmx service url: %s" endpt)
    (.put env
          "java.naming.factory.initial"
          (s/stror contextFactory cfc))
    (let
      [conn (JMXConnectorServerFactory/newJMXConnectorServer
              (JMXServiceURL. endpt)
              env
              (ManagementFactory/getPlatformMBeanServer))]
      (.start conn)
      (c/copy* plug {:conn conn
                     :beanSvr (.getMBeanServer conn) }))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- doReg
  ""
  [^MBeanServer svr ^ObjectName objName ^DynamicMBean mbean]
  (c/doto->> objName
             (.registerMBean svr mbean )
             (log/info "jmx-bean: %s" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable JmsPlugletObj
  xp/JmxPluglet
  (jmx-dereg [me nname]
    (-> ^MBeanServer
        (:beanSvr @me) (.unregisterMBean nname)))
  (jmx-reg [me obj domain nname paths]
    (let [nm (objectName<> domain nname paths)]
      (->> (doReg (:beanSvr @me) nm (bn/jmxBean<> obj))
           (conj (:objNames @me))
           (c/setf! me :objNames))
      nm))
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  Resetable
  (reset [me]
    (doseq [nm (:objNames @me)]
      (c/try! (.jmx-dereg me nm)))
    (c/setf! me :objNames []))
  LifeCycle
  (init [me arg]
    (let [c (get-in @me [:pspec :conf])]
      (->> (b/prevarCfg (merge c arg))
           (c/setf! me :conf))))
  (start [me] (.start me nil))
  (start [me _]
    (doto me startRMI startJMX )
    (log/info "JmxPluglet started"))
  (stop [me]
    (let [^JMXConnectorServer c (:conn @me)
          ^Registry r (:rmi @me)]
      (.reset me)
      (c/try! (some-> c .stop))
      (c/trye!!
        (some-> r
                (UnicastRemoteObject/unexportObject  true)))
      (log/info "JmxPluglet stopped")))
  (dispose [me]
           (.stop me)
           (log/info "JmxPluglet disposed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  specdef
  {:info {:name "JMX Server"
          :version "1.0.0"}
   :conf {:$pluggable ::JmxMonitor
          :registryPort 7777
          :serverPort 7778
          :host ""
          :handler nil}})

;;:host (-> (InetAddress/getLocalHost) .getHostName)})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn JmxMonitor "" [ctr pid]
  (c/mutable<> JmsPlugletObj
               {:pspec (update-in specdef
                                  [:conf]
                                  b/expandVarsInForm)
                :parent ctr
                :emAlias pid
                :objNames [] }))

;; jconsole port
;;(setRegistryPort [_ p] (.setv impl :registryPort p))
;;(setServerPort[_ p] (.setv impl :serverPort p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

