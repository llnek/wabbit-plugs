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

  (:require [czlab.twisty.codec :refer [passwd<>]]
            [czlab.basal.logging :as log])

  (:use [czlab.wabbit.plugs.io.loops]
        [czlab.wabbit.plugs.io.core]
        [czlab.wabbit.base]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [javax.mail.internet MimeMessage]
           [clojure.lang APersistentMap]
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
  (if fd
    (try!
      (if (.isOpen fd) (.close fd true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- closeStore "" [co]

  (let [{:keys [store folder]} @co]
    (closeFolder folder)
    (try! (some-> ^Store store .close))
    (alterStateful co dissoc :store :folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resolveProvider
  ""
  [co ^String cz ^String proto]

  (let
    [mockp (sysProp "wabbit.mock.mail.proto")
     demo? (hgl? mockp)
     proto (if demo? mockp proto)
     kee (keyword proto)
     demop (kee *mock-mail-provider*)
     ss (-> (doto (Properties.)
              (.put  "mail.store.protocol" proto))
            (Session/getInstance nil))
     ps (.getProviders ss)
     [^Provider sun ^String pz]
     (if demo?
       [(Provider. Provider$Type/STORE
                   proto demop "czlab" "1.1.7") demop]
       [(some #(if (= cz
                      (.getClassName ^Provider %)) %)
              (seq ps)) cz])]
    (if (nil? sun)
      (throwIOE (str "Failed to find store: " pz)))
    (log/info "mail store impl = %s" sun)
    (.setProvider ss sun)
    (alterStateful co
                   assoc
                   :proto proto
                   :session ss)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defobject MailMsg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<> "" [co msg]
  (object<> MainMsg
            {:id (str "MailMsg." (seqint2)) :source co :message msg }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- connectPop3 "" [^Config co]

  (let [{:keys [^Session session
                ^String proto]}
        @co
        {:keys [^String host
                ^String user
                port
                passwd]}
        (.config co)]
    (when-some [s (.getStore session proto)]
      (.connect s
                host
                ^long port
                user
                (stror (strit passwd) nil))
      (alterStateful
        me
        assoc
        :store s
        :folder (some-> (.getDefaultFolder s)
                        (.getFolder "INBOX")))
      (let [fd (:folder @me)]
        (when (or (nil? fd)
                  (not (.exists ^Folder fd)))
          (alterStateful me dissoc :store)
          (try! (.close s))
          (throwIOE "cannot find inbox"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- readPop3 "" [^Config co msgs]

  (let [d? (:deleteMsg? (.config co))]
    (doseq [^MimeMessage mm  msgs]
      (doto mm
        (.getAllHeaders)
        (.getContent))
      (dispatch! (evt<> co mm))
      (when d? (.setFlag mm Flags$Flag/DELETED true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanPop3 "" [co]

  (let [{:keys [^Folder folder
                ^Store store]} @co]
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
(defn- sanitize
  ""
  [pkey {:keys [port deleteMsg?
                host user ssl? passwd] :as cfg0}]

  (-> (assoc cfg0 :ssl? (!false? ssl?))
      (assoc :deleteMsg? (true? deleteMsg?))
      (assoc :host (str host))
      (assoc :port (if (spos? port) port 995))
      (assoc :user (str user ))
      (assoc :passwd (text (passwd<> passwd pkey)))))

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
(defn- emailXXX "" [id spec sslvars vars waker]
  {:pspec (update-in spec
                     [:conf] expandVarsInForm)
   :wake waker
   :id id
   :init
   (fn [me arg]
     (let [c (:info (rvtbl (:vtbl @me) :pspec))
           k (-> me getServer pkeyChars)
           c2 (prevarCfg (merge c
                                (sanitize k arg)))]
       (->> (if (:ssl? c2) sslvars vars)
            (resolveProvider me))
       c2))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private connectIMAP "" [co] `(connectPop3 ~co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private scanIMAP "" [co] `(scanPop3 ~co))

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
(defn IMAP "" ^czlab.wabbit.xpis.Pluglet
  ([_ id] (IMAP _ id (IMAPSpec)))
  ([_ id spec]
   (emailXXX id spec [cz-imaps imaps] [cz-imap imap] wakeIMAP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn POP3 "" ^APersistentMap
  ([_ id] (POP3 _ id (POP3Spec)))
  ([_ id spec]
   (emailXXX id spec [cz-pop3s pop3s] [cz-pop3 pop3] wakePOP3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

