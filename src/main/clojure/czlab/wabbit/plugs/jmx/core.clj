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
        [czlab.wabbit.base]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.net InetAddress MalformedURLException]
           [java.rmi.registry LocateRegistry Registry]
           [java.lang.management ManagementFactory]
           [java.rmi.server UnicastRemoteObject]
           [java.rmi NoSuchObjectException]
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
         sb (strbf<>)]
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
    (->> (long (get-in @plug [:conf :registryPort]))
         LocateRegistry/createRegistry
         (setf! me  :rmi ))
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
      (doto plug
        (setf! :beanSvr (.getMBeanServer conn))
        (setf! :conn conn)))))

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
(decl-mutable JmsPlugletObj
  JmxPluglet
  (jmx-dereg [me nname]
    (-> ^MBeanServer
        (:beanSvr @me) (.unregisterMBean nname)))
  (jmx-reg [me obj domain nname paths]
    (let [nm (objectName<> domain nname paths)]
      (->> (doReg (:beanSvr @me) nm (jmxBean<> obj))
           (conj (:objNames @me))
           (setf! me :objNames))
      nm))
  Pluglet
  (get-server [me] (:parent @me))
  (hold-event [_ _ _])
  Idable
  (id [me] (:emAlias @me))
  Resetable
  (reset [me]
    (doseq [nm (:objNames @me)]
      (try! (de-reg me nm)))
    (setf! me :objNames []))
  LifeCycle
  (init [me arg]
    (setf! me :conf (prevarCfg (or arg {}))))
  (start [me] (.start me nil))
  (start [me _]
    (startRMI me)
    (startJMX me)
    (log/info "JmxPluglet started"))
  (stop [me]
    (let [^JMXConnectorServer c (:conn @me)
          ^Registry r (:rmi @me)]
      (.reset me)
      (try! (some-> c .stop ))
      (unsetf! me :conn)
      (try!
        (some-> r
                (UnicastRemoteObject/unexportObject  true)))
      (unsetf! me :rmi)
      (log/info "JmxPluglet stopped")))
  (dispose [_]
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
  (mutable<> JmsPlugletObj
             {:pspec (update-in specdef
                                 [:conf]
                                 expandVarsInForm)
              :parent ctr
              :emAlias pid
              :objNames []}))

;; jconsole port
;;(setRegistryPort [_ p] (.setv impl :registryPort p))
;;(setServerPort[_ p] (.setv impl :serverPort p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

