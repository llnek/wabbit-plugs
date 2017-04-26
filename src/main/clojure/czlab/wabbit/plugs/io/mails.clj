;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for email services."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.mails

  (:require [czlab.twisty.codec :refer [pwd<>]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.plugs.io.loops]
        [czlab.wabbit.plugs.io.core]
        [czlab.wabbit.base]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [javax.mail.internet MimeMessage]
           [clojure.lang APersistentMap]
           [czlab.jasal LifeCycle Idable]
           [javax.mail
            Flags$Flag
            Flags
            Store
            Folder
            Session
            Provider
            Provider$Type]
           [java.util Properties]
           [java.io IOException]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def
  ^:dynamic
  *mock-mail-provider*
  {:pop3s "czlab.proto.mock.mail.MockPop3SSLStore"
   :imaps "czlab.proto.mock.mail.MockIMapSSLStore"
   :pop3 "czlab.proto.mock.mail.MockPop3Store"
   :imap "czlab.proto.mock.mail.MockIMapStore"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POP3
(def ^:private cz-pop3s  "com.sun.mail.pop3.POP3SSLStore")
(def ^:private cz-pop3  "com.sun.mail.pop3.POP3Store")
(def ^:private pop3s "pop3s")
(def ^:private pop3 "pop3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMAP
(def ^:private cz-imaps "com.sun.mail.imap.IMAPSSLStore")
(def ^:private cz-imap "com.sun.mail.imap.IMAPStore")
(def ^:private imaps "imaps")
(def ^:private imap "imap")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- closeFolder "" [^Folder fd]
  (if fd (try! (if (.isOpen fd) (.close fd true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- closeStore "" [co]
  (let [{:keys [store folder]} @co]
    (closeFolder folder)
    (try! (some-> ^Store store .close))
    (unsetf! co dissoc :store)
    (unsetf! co dissoc :folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resolveProvider
  ""
  [co [^String cz ^String proto]]

  (let
    [mockp (sysProp "wabbit.mock.mail.proto")
     demo? (hgl? mockp)
     proto (if demo? mockp proto)
     demop (*mock-mail-provider* (keyword proto))
     ss (-> (doto (Properties.)
              (.put  "mail.store.protocol" proto))
            (Session/getInstance nil))
     [^Provider sun ^String pz]
     (if demo?
       [(Provider. Provider$Type/STORE
                   proto demop "czlab" "1.1.7") demop]
       [(some #(if (= cz (.getClassName ^Provider %)) %)
              (.getProviders ss)) cz])]
    (if (nil? sun)
      (throwIOE (str "Failed to find store: " pz)))
    (log/info "mail store impl = %s" sun)
    (.setProvider ss sun)
    (copy* co {:proto proto :pz pz :session ss})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-object MailMsg
             PlugletMsg
             (get-pluglet [me] (:$source me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private evt<> "" [co msg]
  `(object<> MainMsg
             {:id (str "MailMsg." (seqint2))
              :$source ~co
              :message ~msg }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- connectPop3 "" [co]

  (let [{:keys [^Session session
                ^String proto]}
        @co
        {:keys [^String host
                ^String user
                port
                passwd]}
        (:conf @co)]
    (when-some [s (.getStore session proto)]
      (.connect s
                host
                ^long port
                user
                (stror (strit passwd) nil))
      (copy* co
             {:store s
              :folder (some-> (.getDefaultFolder s)
                              (.getFolder "INBOX"))})
      (let [fd (:folder @co)]
        (when (or (nil? fd)
                  (not (.exists ^Folder fd)))
          (unsetf! co :store)
          (try! (.close s))
          (throwIOE "cannot find inbox"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- readPop3 "" [co msgs]

  (let [d? (get-in @co [:conf :deleteMsg?])]
    (doseq [^MimeMessage mm msgs]
      (doto mm
        (.getAllHeaders)
        (.getContent))
      (dispatch! (evt<> co mm))
      (when d? (.setFlag mm Flags$Flag/DELETED true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanPop3 "" [co]

  (let [{:keys [^Folder folder ^Store store]} @co]
    (if (and folder
             (not (.isOpen folder)))
      (.open folder Folder/READ_WRITE))
    (when (and folder
               (.isOpen folder))
      (try
        (let [cnt (.getMessageCount folder)]
          (log/debug "count of new mail-messages: %d" cnt)
          (if (spos? cnt)
            (readPop3 co (.getMessages folder))))
        (finally
          (try! (.close folder true)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sanitize
  ""
  [pkey {:keys [port deleteMsg?
                host user ssl? passwd] :as cfg0}]

  (-> (assoc cfg0 :ssl? (!false? ssl?))
      (assoc :deleteMsg? (true? deleteMsg?))
      (assoc :host (str host))
      (assoc :port (if (spos? port) port 995))
      (assoc :user (str user ))
      (assoc :passwd (p-text (pwd<> passwd pkey)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable EmailPluglet
  Pluglet
  (hold-event [_ _ _] (throwUOE "email-pluglet:hold-event"))
  (get-server [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me arg]
    (let [k (-> me get-server pkey-chars)
          {:keys [sslvars vars]} @me
          c (get-in @me [:pspec :conf])
          c2 (prevarCfg (sanitize k (merge c arg)))]
      (setf! me :conf c2)
      (resolveProvider me
                       (if (:ssl? c2) sslvars vars))))
  (start [me] (.start me nil))
  (start [me arg]
    (->> (:waker @me)
         (scheduleThreadedLoop me ) (setf! me :loopy )))
  (stop [me]
    (stopThreadedLoop (:loopy @me)))
  (dispose [me] (.stop me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- emailXXX "" [_ id spec sslvars vars wakerFunc]
  (mutable<> EmailPluglet
             {:pspec (update-in spec
                                [:conf]
                                expandVarsInForm)
              :sslvars sslvars
              :vars vars
              :parent _
              :emAlias id
              :waker wakerFunc }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  popspecdef
  {:info {:name "POP3 Client"
          :version "1.0.0"}
   :conf {:$pluggable ::POP3
          :host "pop.gmail.com"
          :port 995
          :deleteMsg? false
          :username "joe"
          :passwd "secret"
          :intervalSecs 300
          :delaySecs 0
          :ssl? true
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn POP3Spec "" ^APersistentMap [] popspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private connectIMAP "" [co] `(connectPop3 ~co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private scanIMAP "" [co] `(scanPop3 ~co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wakePOP3 "" [co]

  (try
    (connectPop3 co)
    (scanPop3 co)
    (catch Throwable _
      (log/exception _))
    (finally
      (closeStore co))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wakeIMAP "" [co]

  (try
    (connectIMAP co)
    (scanIMAP co)
    (catch Throwable _
      (log/exception _))
    (finally
      (closeStore co))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  imapspecdef
  {:info {:name "IMAP Client"
          :version "1.0.0"}
   :conf {:$pluggable ::IMAP
          :host "imap.gmail.com"
          :port 993
          :deleteMsg? false
          :ssl? true
          :username "joe"
          :passwd "secret"
          :intervalSecs 300
          :delaySecs 0
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn IMAPSpec "" ^APersistentMap [] imapspecdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn IMAP ""
  ([_ id] (IMAP _ id (IMAPSpec)))
  ([_ id spec]
   (emailXXX _ id spec [cz-imaps imaps] [cz-imap imap] wakeIMAP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn POP3 ""
  ([_ id] (POP3 _ id (POP3Spec)))
  ([_ id spec]
   (emailXXX _ id spec [cz-pop3s pop3s] [cz-pop3 pop3] wakePOP3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

