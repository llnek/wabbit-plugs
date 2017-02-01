;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.wabbit.plugs.svcs

  (:require [czlab.basal.logging :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.test.wabbit.plugs.mock]
        [czlab.wabbit.plugs.io.core]
        [czlab.wabbit.ctl.core]
        [czlab.convoy.nettio.client]
        [czlab.convoy.net.core]
        [czlab.wabbit.base.core]
        [czlab.flux.wflow.core]
        [czlab.basal.core]
        [czlab.basal.io]
        [czlab.basal.str]
        [clojure.test])

  (:import [java.io DataOutputStream DataInputStream BufferedInputStream]
           [javax.mail Message Message$RecipientType Multipart]
           [javax.mail.internet MimeMessage]
           [javax.jms TextMessage]
           [czlab.convoy.nettio WholeResponse]
           [czlab.flux.wflow WorkStream Job]
           [io.netty.channel Channel]
           [czlab.wabbit.sys Execvisor]
           [czlab.wabbit.plugs.io
            MailMsg
            SocketMsg
            FileMsg
            JmsMsg
            HttpMsg]
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
  #(do->nil
     %
     (swap! result-var + 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn fileHandler
  ""
  []
  #(let [^FileMsg e (.origin ^Job %)
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
       (swap! result-var + n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn sockHandler
  ""
  []
  #(let
     [^SocketMsg ev (.origin ^Job %)
      dis (DataInputStream. (.sockIn ev))
      dos (DataOutputStream. (.sockOut ev))
      nm (.readInt dis)]
     (swap! result-var + nm)
     (.writeInt dos (int nm))
     (.flush dos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jmsHandler
  ""
  []
  #(let [^JmsMsg ev (.origin ^Job %)
         ^TextMessage msg (.message ev)
         s (.getText msg)]
     (assert (hgl? s))
     (swap! result-var + 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn mailHandler
  ""
  []
  #(let [^MailMsg ev (.origin ^Job %)
         ^MimeMessage msg (.message ev)
         _ (assert (some? msg))
         ^Multipart p (.getContent msg)]
     (assert (some? p))
     (swap! result-var + 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn httpHandler
  ""
  []
  (require 'czlab.convoy.nettio.resp)
  #(let [^HttpMsg ev (.origin ^Job %)
         soc (.socket ev)
         res (httpResult<> soc (.msgGist ev))]
     (.setContentType res "text/plain")
     (.setContent res "hello")
     (replyResult soc res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestwabbitplugs-svcs

  (is (let [_ (sysProp! "wabbit.mock.mail.proto" "imaps")
            etype :czlab.wabbit.plugs.io.mails/IMAP
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s
               {:handler "czlab.test.wabbit.plugs.svcs/mailHandler"
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
            etype :czlab.wabbit.plugs.io.mails/POP3
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s
               {:handler "czlab.test.wabbit.plugs.svcs/mailHandler"
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

  (is (let [etype :czlab.wabbit.plugs.io.http/HTTP
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (.init s
               {:handler "czlab.test.wabbit.plugs.svcs/httpHandler"
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

  (is (let [_ (sysProp! "wabbit.mock.jms.loopsecs" "1")
            etype :czlab.wabbit.plugs.io.jms/JMS
            c {:contextFactory "czlab.proto.mock.jms.MockContextFactory"
               :providerUrl "java://aaa"
               ;;:connFactory "tcf"
               ;;:destination "topic.abc"
               :connFactory "qcf"
               :destination "queue.xyz"
               :jndiUser "root"
               :jndiPwd "root"
               :jmsUser "anonymous"
               :jmsPwd "anonymous"
               :handler "czlab.test.wabbit.plugs.svcs/jmsHandler"}
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s nil)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [_ (sysProp! "wabbit.mock.jms.loopsecs" "1")
            etype :czlab.wabbit.plugs.io.jms/JMS
            c {:contextFactory "czlab.proto.mock.jms.MockContextFactory"
               :providerUrl "java://aaa"
               ;;:connFactory "tcf"
               ;;:destination "topic.abc"
               :connFactory "qcf"
               :destination "queue.xyz"
               :jndiUser "root"
               :jndiPwd "root"
               :jmsUser "anonymous"
               :jmsPwd "anonymous"
               :handler "czlab.test.wabbit.plugs.svcs/jmsHandler"}
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s nil)
        (pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.plugs.io.socket/SocketIO
            host "localhost"
            port 5555
            c {:host host
               :port port
               :handler "czlab.test.wabbit.plugs.svcs/sockHandler"}
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (.init s c)
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

  (is (let [etype :czlab.wabbit.plugs.io.loops/OnceTimer
            c {:delaySecs 1
               :handler "czlab.test.wabbit.plugs.svcs/testHandler"}
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s nil)
        (pause 2000)
        (.stop s)
        (.dispose ctr)
        (== 8 @result-var)))

  (is (let [etype :czlab.wabbit.plugs.io.loops/RepeatingTimer
            c {:delaySecs 1
               :intervalSecs 1
               :handler "czlab.test.wabbit.plugs.svcs/testHandler"}
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s nil)
        (pause 3500)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.plugs.io.files/FilePicker
            root *tempfile-repo*
            from (str root "/from")
            to (str root "/to")
            firstfn (str from "/" (juid))
            c {:targetFolder from
               :recvFolder to
               :fmask ""
               :intervalSecs 1
               :delaySecs 0
               :handler "czlab.test.wabbit.plugs.svcs/fileHandler"}
            ctr (mocker :exec)
            s (plugletViaType<> ctr etype "t")]
        (.init s c)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


