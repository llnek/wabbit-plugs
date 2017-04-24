;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for FilePicker."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.io.files

  (:require [czlab.basal.io :refer [mkdirs]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io])

  (:use [czlab.wabbit.plugs.io.loops]
        [czlab.wabbit.plugs.io.core]
        [czlab.wabbit.base]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.io FileFilter File IOException]
           [java.util Properties ResourceBundle]
           [clojure.lang APersistentMap]
           [czlab.jasal Config]
           [org.apache.commons.io.filefilter
            SuffixFileFilter
            PrefixFileFilter
            RegexFileFilter
            FileFileFilter]
           [org.apache.commons.io.monitor
            FileAlterationListener
            FileAlterationMonitor
            FileAlterationObserver
            FileAlterationListenerAdaptor]
           [org.apache.commons.io FileUtils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* false)

(decl-object FileMsg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ""
  [co fname fp action]

  (object<> FileMsg
            {:file (io/file fp)
             :source co
             :originalFileName fname
             :id (str "FileMsg." (seqint2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- postPoll
  "Only look for new files"
  [plug {:keys [recvFolder]} ^File f action]
  (let
    [orig (.getName f)]
    (if-some
      [cf (if (and (not= action :FP-DELETED)
                   (some? recvFolder))
            (try! (doto->> (io/file recvFolder orig)
                           (FileUtils/moveFile f))))]
      (dispatch! (evt<> plug orig cf action)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- toFMask
  "" ^FileFilter [^String mask]
  (cond
    (.startsWith mask "*.")
    (SuffixFileFilter. (.substring mask 1))
    (.endsWith mask "*")
    (PrefixFileFilter.
      (.substring mask
                  0
                  (dec (.length mask))))
    (> (.length mask) 0)
    (RegexFileFilter. mask)
    :else
    FileFileFilter/FILE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- init2
  "" [conf cfg0]
  (let
    [{:keys [recvFolder
             fmask
             targetFolder] :as c2}
     (merge conf cfg0)
     root targetFolder
     dest recvFolder
     ff (toFMask (str fmask))]
    (test-hgl "file-root-folder" root)
    (log/info
      (str "monitoring folder: %s\n"
           "rcv folder: %s") root (nsn dest))
    (merge c2 {:targetFolder root
               :fmask ff
               :recvFolder dest})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- fileMon<>
  ""
  ^FileAlterationMonitor
  [^Config plug]
  (let
    [{:keys [targetFolder
             intervalSecs
             ^FileFilter fmask] :as cfg}
     (.config me)
     obs (-> (io/file targetFolder)
             (FileAlterationObserver. fmask))
     mon (-> (s2ms intervalSecs) FileAlterationMonitor.)]
    (->>
      (proxy [FileAlterationListenerAdaptor][]
        (onFileCreate [f]
          (postPoll plug cfg f :FP-CREATED))
        (onFileChange [f]
          (postPoll plug cfg f :FP-CHANGED))
        (onFileDelete [f]
          (postPoll plug cfg f :FP-DELETED)))
      (.addListener obs ))
    (.addObserver mon obs)
    mon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  specdef
  {:info {:name "File Picker"
          :version "1.0.0"}
   :conf {:$pluggable ::FilePicker
          :targetFolder "/home/dropbox"
          :recvFolder "/home/joe"
          :fmask ""
          :intervalSecs 300
          :delaySecs 0
          :handler nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn FilePickerSpec "" ^APersistentMap [] specdef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn FilePicker "" ^APersistentMap

  ([_ id] (FilePicker _ id (FilePickerSpec)))
  ([_ id spec]
   {:pspec (update-in spec
                      [:conf] expandVarsInForm)
    :id id
    :schedule
    (fn [me _]
      (log/info "apache io monitor starting...")
      (doto->> (fileMon<> me)
               (setf! me :mon ) .start))
    :id id
    :init
    (fn [me carg]
      (-> (:vtbl @me) (rvtbl :pspec) :conf (init2 carg) prevarCfg ))
    :stop
    (fn [me]
      (log/info "apache io monitor stopping...")
      (some-> ^FileAlterationMonitor
              (unsetf! me :mon) .stop))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

