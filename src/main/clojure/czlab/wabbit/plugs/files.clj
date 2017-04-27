;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for FilePicker."
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.files

  (:require [czlab.basal.io :refer [mkdirs]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io])

  (:use [czlab.wabbit.plugs.loops]
        [czlab.wabbit.plugs.core]
        [czlab.wabbit.base]
        [czlab.wabbit.xpis]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [czlab.jasal Hierarchical LifeCycle Idable]
           [java.util Timer Properties ResourceBundle]
           [java.io FileFilter File IOException]
           [clojure.lang APersistentMap]
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

(decl-object FileMsg
             PlugletMsg
             (get-pluglet [me] (:$source me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private evt<>
  ""
  [co fname fp action]
  `(object<> FileMsg
             {:file (io/file ~fp)
              :$source ~co
              :originalFileName ~fname
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
  [plug]
  (let
    [{:keys [targetFolder
             recvFolder
             intervalSecs
             ^FileFilter fmask]}
     (:conf @plug)
     obs (-> (io/file targetFolder)
             (FileAlterationObserver. fmask))
     mon (-> (s2ms intervalSecs) FileAlterationMonitor.)]
    (->>
      (proxy [FileAlterationListenerAdaptor][]
        (onFileCreate [f]
          (postPoll plug recvFolder f :FP-CREATED))
        (onFileChange [f]
          (postPoll plug recvFolder f :FP-CHANGED))
        (onFileDelete [f]
          (postPoll plug recvFolder f :FP-DELETED)))
      (.addListener obs ))
    (.addObserver mon obs)
    mon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable FilePickerPluglet
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me carg]
    (-> (get-in @me [:pspec :conf])
        (init2 carg)
        (prevarCfg)
        (setf! me :conf)))
  (start [me] (.start me nil))
  (start [me arg]
    (let [w #(doto->> (fileMon<> me) (setf! me :mon) .start)]
      (log/info "apache io monitor starting...")
      (->> (cfgTimer (Timer. true) w (:conf @me) false)
           (setf! me :ttask))))
  (dispose [me]
           (.stop me))
  (stop [me]
    (log/info "apache io monitor stopping...")
    (cancelTimerTask (:ttask @me))
    (some-> ^FileAlterationMonitor (:mon @me) .stop)))

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
(defn FilePicker ""

  ([_ id] (FilePicker _ id (FilePickerSpec)))
  ([_ id spec]
   (mutable<> FilePickerPluglet
              {:pspec (update-in spec
                                 [:conf] expandVarsInForm)
               :parent _
               :emAlias id })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

