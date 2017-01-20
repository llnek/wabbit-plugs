;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.wabbit.test

  (:require [czlab.xlib.logging :as log]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.wabbit.cons.con1]
        [czlab.wabbit.cons.con2]
        [czlab.wabbit.cons.con8]
        [czlab.wabbit.cons.core]
        [czlab.wabbit.common.core]
        [czlab.xlib.core]
        [czlab.xlib.io]
        [czlab.xlib.str]
        [clojure.test])

  (:import [czlab.wabbit.cons CmdError]
           [java.io File ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestwabbit-test

  (is (= "hello"
         (gtid (with-meta {:a 1} {:typeid "hello"}))))

  (is (> (count (expandSysProps "${user.home}")) 0))
  (is (> (count (expandEnvVars "${HOME}")) 0))
  (is (= (str (expandSysProps "${user.home}")
              (expandEnvVars "${HOME}"))
         (expandVars "${user.home}${HOME}")))

  (is (precondDir *tempfile-repo*))
  (is (let [t (tempFile)
            _ (spitUtf8 t "hello")
            ok (precondFile t)]
        (deleteQ t)
        ok))

  (is (let [m (muble<> {:s "hello.txt"
                        :f (io/file "hello.txt")})]
        (and (inst? File (maybeDir m :s))
             (inst? File (maybeDir m :f)))))

  (is (let [fp (fpath *tempfile-repo*)
            _ (sysProp! "wabbit.proc.dir" fp)
            t (tempFile)
            _ (spitUtf8 t "${pod.dir}")
            s (readConf t)]
        (deleteQ t)
        (= s fp)))

  (is (let [fp (fpath *tempfile-repo*)
            tn (juid)
            _ (spitXXXConf fp tn {:a 1})
            m (slurpXXXConf fp tn)]
        (deleteQ (io/file fp tn))
        (and (== 1 (count m))
             (== 1 (:a m)))))

  (is (let [fp (fpath *tempfile-repo*)
            _ (sysProp! "wabbit.proc.dir" fp)
            tn (juid)
            _ (spitXXXConf fp tn {:a "${pod.dir}"})
            m (slurpXXXConf fp tn true)]
        (deleteQ (io/file fp tn))
        (and (== 1 (count m))
             (string? (:a m))
             (> (count (:a m)) 0))))

  (is (== 17 (-> (with-out-str
                   (onGenerate ["--password" "17"] ))
                 (trimr "\n")
                 count)))
  (is (== 13 (-> (with-out-str
                   (onGenerate ["-p" "13"] ))
                 (trimr "\n")
                 count)))

  (is (> (-> (with-out-str
               (onGenerate ["--hash" "hello"]))
             (trimr "\n")
             count) 0))
  (is (> (-> (with-out-str
               (onGenerate ["-h" "hello"]))
             (trimr "\n")
             count) 0))

  (is (> (-> (with-out-str
               (onGenerate ["--uuid"]))
             (trimr "\n")
             count) 0))
  (is (> (-> (with-out-str
               (onGenerate ["-u"]))
             (trimr "\n")
             count) 0))

  (is (> (-> (with-out-str
               (onGenerate ["--wwid"]))
             (trimr "\n")
             count) 0))
  (is (> (-> (with-out-str
               (onGenerate ["-w"]))
             (trimr "\n")
             count) 0))

  (is (let [e (-> (with-out-str
                    (onGenerate ["--encrypt" "secret" "hello"]))
                  (trimr "\n"))
            d (-> (with-out-str
                    (onGenerate ["--decrypt" "secret" e]))
                  (trimr "\n"))]
        (= d "hello")))

  (is (let [e (-> (with-out-str
                    (onGenerate ["-e" "secret" "hello"]))
                  (trimr "\n"))
            d (-> (with-out-str
                    (onGenerate ["-d" "secret" e]))
                  (trimr "\n"))]
        (= d "hello")))

  (is (thrown? CmdError (onGenerate ["-bbbbb"])))

  (is (let [p (fpath "/wdrive/tmp");;*tempfile-repo*)
            _ (sysProp! "wabbit.home.dir" (fpath (getCwd)))
            _ (sysProp! "wabbit.proc.dir" p)
            _ (deleteDir (io/file p "web"))
            _ (onCreate ["-w" "web"])]
        true))

  (is (let [p (fpath "/wdrive/tmp");;*tempfile-repo*)
            _ (sysProp! "wabbit.home.dir" (fpath (getCwd)))
            _ (sysProp! "wabbit.proc.dir" p)
            _ (deleteDir (io/file p "soa"))
            _ (onCreate ["-s" "soa"])]
          true))


  (is (string? "That's all folks!")))

;;(clojure.test/run-tests 'czlab.test.wabbit.test)

