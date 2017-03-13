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
        [czlab.wabbit.base.core]
        [czlab.basal.core]
        [czlab.basal.str]
        [czlab.wabbit.plugs.io.core])

  (:import [javax.mail.internet MimeMessage]
           [czlab.wabbit.plugs.io MailMsg]
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
           [java.io IOException]
           [czlab.wabbit.ctl Pluglet Pluggable]))

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
(def ^:private pop3c "pop3")

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
(defn- closeStore "" [^Pluglet co]

  (let [{:keys [store folder]}
        (.intern (.getx co))]
    (closeFolder folder)
    (if store
      (try!
        (.close ^Store store)))
    (doto (.getx co)
      (.unsetv :store)
      (.unsetv :folder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- resolveProvider
  ""
  [^Pluglet co ^String cz ^String proto]

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
    (doto (.getx co)
      (.setv :proto proto)
      (.setv :session ss))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  "" [^Pluglet co {:keys [msg]}]

  (let [eeid (str "event#" (seqint2))]
    (with-meta
      (reify MailMsg
        (id [_] eeid)
        (source [_] co)
        (message [_] msg))
      {:typeid ::MailMsg })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- connectPop3 "" [^Pluglet co]

  (let [{:keys [^Session session
                ^String proto]}
        (.intern (.getx co))
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
                (stror (str passwd) nil))
      (doto (.getx co)
        (.setv :folder
               (some-> (.getDefaultFolder s)
                       (.getFolder "INBOX")))
        (.setv :store s))
      (let [fd (.getv (.getx co) :folder)]
        (when (or (nil? fd)
                  (not (.exists ^Folder fd)))
          (.unsetv (.getx co) :store)
          (try! (.close s))
          (throwIOE "cannot find inbox"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- readPop3 "" [^Pluglet co msgs]

  (let [d? (.getv (.getx co) :deleteMsg?)]
    (doseq [^MimeMessage mm  msgs]
      (doto mm
        (.getAllHeaders)
        (.getContent))
      (dispatch! (evt<> co {:msg mm}))
      (when d? (.setFlag mm Flags$Flag/DELETED true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- scanPop3 "" [^Pluglet co]

  (let [{:keys [^Folder folder ^Store store]}
        (.intern (.getx co))]
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
(defn- wake<o> "" [^Pluglet co]

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
      (assoc :passwd (.text (passwd<> passwd pkey)))))

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
(defn POP3 "" ^Pluggable

  ([_ id] (POP3 _ id (POP3Spec)))
  ([_ id spec]
   (let
     [{:keys [conf] :as pspec}
      (update-in spec [:conf] expandVarsInForm)
      impl (muble<>)]
     (reify Pluggable
       (setParent [_ p] (.setv impl :$parent p))
       (parent [_] (.getv impl :$parent))
       (config [_] (dissoc (.intern impl)
                           :$funcs :$parent))
       (spec [_] pspec)
       (start [_ _]
         ((:$start (.getv impl :$funcs)) (.intern impl)))
       (stop [_]
         ((:$stop (.getv impl :$funcs))))
       (init [this arg]
         (let [^Pluglet pg (.parent this)
               k (.. pg server pkey)
               c2 (merge conf (sanitize k arg))
               [z p] (if (:ssl? c2)
                       [cz-pop3s pop3s]
                       [cz-pop3 pop3c])]
           (->> (threadedTimer {:$wakeup #(wake<o> pg)})
                (.setv impl :$funcs))
           (.copyEx impl (prevarCfg c2))
           (resolveProvider pg z p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private connectIMAP "" [co] `(connectPop3 ~co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private scanIMAP "" [co] `(scanPop3 ~co))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- wake<i> "" [co]

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
(defn IMAP "" ^Pluggable

  ([_ id] (IMAP _ id (IMAPSpec)))
  ([_ id spec]
   (let
     [{:keys [conf] :as pspec}
      (update-in spec [:conf] expandVarsInForm)
      impl (muble<>)]
     (reify Pluggable
       (setParent [_ p] (.setv impl :$parent p))
       (parent [_] (.getv impl :$parent))
       (config [_] (dissoc (.intern impl)
                           :$funcs :$parent))
       (spec [_] pspec)
       (start [_ _]
         ((:$start (.getv impl :$funcs)) (.intern impl)))
       (stop [_]
         ((:$stop (.getv impl :$funcs))))
       (init [this arg]
         (let [^Pluglet pg (.parent this)
               k (.. pg server pkey)
               c2 (merge conf (sanitize k arg))
               [z p] (if (:ssl? c2)
                       [cz-imaps imaps]
                       [cz-imap imap])]
           (->> (threadedTimer {:$wakeup #(wake<i> pg)})
                (.setv impl :$funcs))
           (.copyEx impl (prevarCfg c2))
           (resolveProvider pg z p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

