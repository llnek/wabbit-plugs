;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.wabbit.svcs

  (:require [czlab.xlib.logging :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.convoy.netty.client]
        [czlab.convoy.net.core]
        ;;[czlab.convoy.netty.resp]
        [czlab.test.wabbit.mock]
        [czlab.wabbit.common.svcs]
        [czlab.wabbit.common.core]
        [czlab.wabbit.cons.core]
        [czlab.wabbit.etc.core]
        [czlab.wabbit.io.core]
        [czlab.flux.wflow.core]
        [czlab.xlib.core]
        [czlab.xlib.io]
        [czlab.xlib.str]
        [clojure.test])

  (:import [java.io DataOutputStream DataInputStream BufferedInputStream]
           [czlab.wabbit.io
            EmailEvent
            SocketEvent
            FileEvent
            JmsEvent
            HttpEvent]
           [czlab.flux.wflow WorkStream Job]
           [io.netty.channel Channel]
           [czlab.wabbit.server Container]
           [javax.mail Message Message$RecipientType Multipart]
           [javax.mail.internet MimeMessage]
           [javax.jms TextMessage]
           [czlab.convoy.netty WholeResponse]
           [java.net Socket]
           [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private result-var (atom 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn testHandler
  ""
  []
  (workStream<>
    (script<>
      (fn [_ _]
        (do->nil
          (swap! result-var + 8))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileHandler
  ""
  []
  (workStream<>
    (script<>
      (fn [_ ^Job job]
        (let [^FileEvent e (.event job)
              {:keys [targetFolder recvFolder]}
              (.. e source config)
              tp (fpath targetFolder)
              rp (fpath recvFolder)
              nm (juid)
              f (.file e)
              fp (fpath f)
              s (slurpUtf8 f)
              n (convLong s 0)]
          ;;the file should be in the recv-folder
          (when (>= (.indexOf fp rp) 0)
            ;; generate a new file in target-folder
            (spitUtf8 (io/file tp nm) s)
            (swap! result-var + n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn sockHandler
  ""
  []
  (workStream<>
    (script<>
      #(let
         [^Job job %2
          ^SocketEvent ev (.event job)
          dis (DataInputStream. (.sockIn ev))
          dos (DataOutputStream. (.sockOut ev))
          nm (.readInt dis)]
         (swap! result-var + nm)
         (.writeInt dos (int nm))
         (.flush dos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jmsHandler
  ""
  []
  (workStream<>
    (script<>
      #(let [^JmsEvent ev (.event ^Job %2)
             ^TextMessage msg (.message ev)
             s (.getText msg)]
         (assert (hgl? s))
         (swap! result-var + 8)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mailHandler
  ""
  []
  (workStream<>
    (script<>
      #(let [^EmailEvent ev (.event ^Job %2)
             ^MimeMessage msg (.message ev)
             _ (assert (some? msg))
             ^Multipart p (.getContent msg)]
         (assert (some? p))
         (swap! result-var + 8)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn httpHandler
  ""
  []
  (require 'czlab.convoy.netty.resp)
  (workStream<>
    (script<>
      #(let [^HttpEvent ev (.event ^Job %2)
             soc (.socket ev)
             res (httpResult<> soc (.msgGist ev))]
         (.setContentType res "text/plain")
         (.setContent res "hello")
         (replyResult soc res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestwabbit-svcs

  (is (let [etype :czlab.wabbit.io.http/HTTP
            m (emitterByService etype)
            c (:conf m)
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (.init s
               {:handler "czlab.test.wabbit.svcs/httpHandler"
                :host "localhost"
                :port 8888})
        (.start s nil)
        (pause 1000)
        (let [res (h1get "http://localhost:8888/test/get/xxx")
              ^WholeResponse
              rc (deref res 2000 nil)
              z (some-> rc
                        (.content ) (.stringify))]
          (.stop s)
          (.dispose s)
          (.dispose ctr)
          (= "hello" z))))

  (is (let [_ (sysProp! "wabbit.mock.mail.proto" "imaps")
            etype :czlab.wabbit.io.mails/IMAP
            m (emitterByService etype)
            c (:conf m)
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (reset! result-var 0)
        (.init s
               {:handler "czlab.test.wabbit.svcs/mailHandler"
                :host "localhost"
                :port 7110
                :intervalSecs 1
                :username "test1"
                :passwd "secret"})
        (.start s nil)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [_ (sysProp! "wabbit.mock.mail.proto" "pop3s")
            etype :czlab.wabbit.io.mails/POP3
            m (emitterByService etype)
            c (:conf m)
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (reset! result-var 0)
        (.init s
               {:handler "czlab.test.wabbit.svcs/mailHandler"
                :host "localhost"
                :port 7110
                :intervalSecs 1
                :username "test1"
                :passwd "secret"})
        (.start s nil)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [_ (sysProp! "wabbit.mock.jms.loopsecs" "1")
            etype :czlab.wabbit.io.jms/JMS
            m (emitterByService etype)
            c (assoc (:conf m)
                     :contextFactory "czlab.wabbit.mock.jms.MockContextFactory"
                     :providerUrl "java://aaa"
                     ;;:connFactory "tcf"
                     ;;:destination "topic.abc"
                     :connFactory "qcf"
                     :destination "queue.xyz"
                     :handler "czlab.test.wabbit.svcs/jmsHandler")
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (reset! result-var 0)
        (.init s
               {:jndiUser "root"
                :jndiPwd "root"
                :jmsUser "anonymous"
                :jmsPwd "anonymous"})
        (.start s nil)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [_ (sysProp! "wabbit.mock.jms.loopsecs" "1")
            etype :czlab.wabbit.io.jms/JMS
            m (emitterByService etype)
            c (assoc (:conf m)
                     :contextFactory "czlab.wabbit.mock.jms.MockContextFactory"
                     :providerUrl "java://aaa"
                     ;;:connFactory "tcf"
                     ;;:destination "topic.abc"
                     :connFactory "qcf"
                     :destination "queue.xyz"
                     :handler "czlab.test.wabbit.svcs/jmsHandler")
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (reset! result-var 0)
        (.init s
               {:jndiUser "root"
                :jndiPwd "root"
                :jmsUser "anonymous"
                :jmsPwd "anonymous"})
        (.start s nil)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.io.socket/Socket
            m (emitterByService etype)
            host "localhost"
            port 5555
            c (assoc (:conf m)
                     :host host
                     :port port
                     :handler "czlab.test.wabbit.svcs/sockHandler")
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (.init s {})
        (.start s nil)
        (reset! result-var 0)
        (dotimes [n 2]
          (pause 1000)
          (with-open [soc (Socket. host (int port))]
             (let [os (.getOutputStream soc)
                   is (.getInputStream soc)
                   dis (DataInputStream. is)]
               (doto (DataOutputStream. os)
                 (.writeInt (int 8))
                 (.flush))
               (let [nm (.readInt dis)]
                 (swap! result-var + nm)))))
        (.stop s)
        (.dispose ctr)
        (== @result-var 32)))


  (is (let [etype :czlab.wabbit.io.loops/OnceTimer
            m (emitterByService etype)
            c (assoc (:conf m)
                     :delaySecs 1
                     :handler "czlab.test.wabbit.svcs/testHandler")
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (reset! result-var 0)
        (.start s nil)
        (pause 2000)
        (.stop s)
        (.dispose ctr)
        (== 8 @result-var)))

  (is (let [etype :czlab.wabbit.io.loops/RepeatingTimer
            m (emitterByService etype)
            c (assoc (:conf m)
                     :delaySecs 1
                     :intervalSecs 1
                     :handler "czlab.test.wabbit.svcs/testHandler")
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (reset! result-var 0)
        (.start s nil)
        (pause 3500)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.io.files/FilePicker
            m (emitterByService etype)
            root *tempfile-repo*
            from (str root "/from")
            to (str root "/to")
            firstfn (str from "/" (juid))
            c (assoc (:conf m)
                     :targetFolder from
                     :recvFolder to
                     :fmask ""
                     :intervalSecs 1
                     :delaySecs 0
                     :handler "czlab.test.wabbit.svcs/fileHandler")
            ^Container
            ctr (mock :container)
            s (service<> ctr etype "t" c)]
        (.init s {})
        (deleteDir from)
        (deleteDir to)
        (mkdirs from)
        (mkdirs to)
        (spitUtf8 firstfn "8")
        (reset! result-var 0)
        (.start s nil)
        (pause 1000)
        (touch! firstfn)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (deleteDir from)
        (deleteDir to)
        (> @result-var 8)))



  (is (string? "That's all folks!")))

;;(clojure.test/run-tests 'czlab.test.wabbit.svcs)


