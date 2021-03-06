;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.wabbit.plugs.svcs

  (:require [czlab.test.wabbit.plugs.mock :as mk]
            [czlab.wabbit.plugs.core :as pc]
            [czlab.basal.log :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [czlab.nettio.client :as cl]
            [czlab.wabbit.xpis :as xp]
            [czlab.nettio.core :as nc]
            [czlab.convoy.core :as cc]
            [czlab.wabbit.base :as b]
            [czlab.basal.core :as c]
            [czlab.basal.io :as i]
            [czlab.basal.str :as s])

  (:use [clojure.test])

  (:import [java.io DataOutputStream DataInputStream BufferedInputStream]
           [javax.mail Message Message$RecipientType Multipart]
           [javax.mail.internet MimeMessage]
           [io.netty.channel Channel]
           [czlab.jasal XData]
           [javax.jms TextMessage]
           [java.net Socket]
           [java.io File]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def ^:private result-var (atom 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- testHandler "" [evt] (c/do->nil (swap! result-var + 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- fileHandler "" [evt]
  (let [{:keys [targetFolder recvFolder]}
        (:conf @(xp/get-pluglet evt))
        tp (c/fpath targetFolder)
        rp (c/fpath recvFolder)
        nm (c/jid<>)
        f (:file evt)
        fp (c/fpath f)
        s (i/slurpUtf8 f)
        n (c/convLong s 0)]
    ;;the file should be in the recv-folder
    (when (>= (.indexOf fp rp) 0)
      ;; generate a new file in target-folder
      (i/spitUtf8 (io/file tp nm) s)
      (swap! result-var + n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- sockHandler "" [evt]
  (let [dis (DataInputStream. (:sockin evt))
        dos (DataOutputStream. (:sockout evt))
        nm (.readInt dis)]
    (swap! result-var + nm)
    (.writeInt dos (int nm))
    (.flush dos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- jmsHandler "" [evt]
  (let [^TextMessage msg (:message evt)
        s (.getText msg)]
    (assert (s/hgl? s))
    (swap! result-var + 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mailHandler "" [evt]
  (let [^MimeMessage msg (:message evt)
        _ (assert (some? msg))
        ^Multipart p (.getContent msg)]
    (assert (some? p))
    (swap! result-var + 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- httpHandler "" [evt res]
  (require 'czlab.nettio.resp)
  (let [cfg (:conf @(xp/get-pluglet evt))
        ch (:socket evt)]
    (nc/setHeader (:headers res) "content-type" "text/plain")
    (-> (assoc res :body "hello")
        cc/reply-result )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestwabbitplugs-svcs

  (is (c/do->true (c/sysProp! "wabbit.user.dir" (c/fpath i/*tempfile-repo*))))

  (is (let [_ (c/sysProp! "wabbit.mock.mail.proto" "pop3s")
            etype :czlab.wabbit.plugs.mails/POP3
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s
               {:handler #'czlab.test.wabbit.plugs.svcs/mailHandler
                :host "localhost"
                :port 7110
                :intervalSecs 1
                :username "test1"
                :passwd "secret"})
        (.start s)
        (c/pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [_ (c/sysProp! "wabbit.mock.mail.proto" "imaps")
            etype :czlab.wabbit.plugs.mails/IMAP
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s
               {:handler #'czlab.test.wabbit.plugs.svcs/mailHandler
                :host "localhost"
                :port 7110
                :intervalSecs 1
                :username "test1"
                :passwd "secret"})
        (.start s)
        (c/pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.plugs.http/HTTP
            ctr (mk/mocker :exec)
            dir (c/sysProp "wabbit.user.dir")
            s (xp/plugletViaType<> ctr etype "t")]
        (.init s
               {:handler #'czlab.test.wabbit.plugs.svcs/httpHandler
                :host "localhost"
                :port 8888})
        (.start s)
        (c/pause 1000)
        (let [f (io/file dir "public/test.js")
              _ (.mkdirs (.getParentFile f))
              _ (spit f "hello")
              res (cl/h1get "http://localhost:8888/public/test.js")
              rc (deref res 2000 nil)
              z (some-> ^XData (:body rc) .strit)]
          (.stop s)
          (.dispose s)
          (.dispose ctr)
          (= "hello" z))))

  (is (let [etype :czlab.wabbit.plugs.http/HTTP
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (.init s
               {:handler #'czlab.test.wabbit.plugs.svcs/httpHandler
                :routes nil
                :host "localhost"
                :port 8888})
        (.start s)
        (c/pause 1000)
        (let [res (cl/h1get "http://localhost:8888/test/get/xxx")
              rc (deref res 2000 nil)
              z (some-> ^XData (:body rc) .strit)]
          (.stop s)
          (.dispose s)
          (.dispose ctr)
          (= "hello" z))))

  (is (let [_ (c/sysProp! "wabbit.mock.jms.loopsecs" "1")
            etype :czlab.wabbit.plugs.jms/JMS
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
               :handler #'czlab.test.wabbit.plugs.svcs/jmsHandler}
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s)
        (c/pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [_ (c/sysProp! "wabbit.mock.jms.loopsecs" "1")
            etype :czlab.wabbit.plugs.jms/JMS
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
               :handler #'czlab.test.wabbit.plugs.svcs/jmsHandler}
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s)
        (c/pause 3000)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.plugs.socket/SocketIO
            host "localhost"
            port 5555
            c {:host host
               :port port
               :handler #'czlab.test.wabbit.plugs.svcs/sockHandler}
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (.init s c)
        (.start s)
        (reset! result-var 0)
        (dotimes [n 2]
          (c/pause 1000)
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

  (is (let [etype :czlab.wabbit.plugs.loops/OnceTimer
            c {:delaySecs 1
               :handler #'czlab.test.wabbit.plugs.svcs/testHandler}
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s)
        (c/pause 2000)
        (.stop s)
        (.dispose ctr)
        (== 8 @result-var)))

  (is (let [etype :czlab.wabbit.plugs.loops/RepeatingTimer
            c {:delaySecs 1
               :intervalSecs 1
               :handler #'czlab.test.wabbit.plugs.svcs/testHandler}
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (reset! result-var 0)
        (.init s c)
        (.start s)
        (c/pause 3500)
        (.stop s)
        (.dispose ctr)
        (> @result-var 8)))

  (is (let [etype :czlab.wabbit.plugs.files/FilePicker
            root i/*tempfile-repo*
            from (str root "/from")
            to (str root "/to")
            firstfn (str from "/" (c/jid<>))
            c {:targetFolder from
               :recvFolder to
               :fmask ""
               :intervalSecs 1
               :delaySecs 0
               :handler #'czlab.test.wabbit.plugs.svcs/fileHandler}
            ctr (mk/mocker :exec)
            s (xp/plugletViaType<> ctr etype "t")]
        (.init s c)
        (b/deleteDir from)
        (b/deleteDir to)
        (i/mkdirs from)
        (i/mkdirs to)
        (i/spitUtf8 firstfn "8")
        (reset! result-var 0)
        (.start s)
        (c/pause 1500)
        (i/touch! firstfn)
        (c/pause 3000)
        (.stop s)
        (.dispose ctr)
        (b/deleteDir from)
        (b/deleteDir to)
        (> @result-var 8)))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

