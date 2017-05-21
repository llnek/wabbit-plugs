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

  (:require [czlab.basal.io :as i :refer [mkdirs]]
            [czlab.basal.log :as log]
            [clojure.java.io :as io]
            [czlab.wabbit.plugs.loops :as pl]
            [czlab.wabbit.plugs.core :as pc]
            [czlab.wabbit.base :as b]
            [czlab.wabbit.xpis :as xp]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

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

(c/decl-object FileMsg
               xp/PlugletMsg
               (get-pluglet [me] (:$source me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private evt<>
  ""
  [co fname fp action]
  `(c/object<> FileMsg
               {:file (io/file ~fp)
                :$source ~co
                :originalFileName ~fname
                :id (str "FileMsg." (c/seqint2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- postPoll
  "Only look for new files"
  [plug recvFolder ^File f action]
  (let
    [orig (.getName f)]
    (when-some
      [cf (if (and (not= action :FP-DELETED)
                   (some? recvFolder))
            (c/try! (c/doto->> (io/file recvFolder orig)
                               (FileUtils/moveFile f))))]
      (pc/dispatch! (evt<> plug orig cf action)))))

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
    (c/test-hgl "file-root-folder" root)
    (log/info
      (str "monitoring folder: %s\n"
           "rcv folder: %s") root (s/nsn dest))
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
             ^FileFilter fmask] :as cfg}
     (:conf @plug)
     obs (-> (io/file targetFolder)
             (FileAlterationObserver. fmask))]
    (c/do-with
      [mon (-> (pc/s2ms intervalSecs)
               FileAlterationMonitor.)]
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
      (.start mon))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-mutable FilePickerPluglet
  Hierarchical
  (parent [me] (:parent @me))
  Idable
  (id [me] (:emAlias @me))
  LifeCycle
  (init [me carg]
    (->> (-> (get-in @me [:pspec :conf])
             (init2 carg)
             (b/prevarCfg))
         (c/setf! me :conf)))
  (start [me] (.start me nil))
  (start [me arg]
    (let [w #(->> (fileMon<> me) (c/setf! me :mon))]
      (log/info "apache io monitor starting...")
      (->> (pl/cfgTimer (Timer. true) w (:conf @me) false)
           (c/setf! me :ttask))))
  (dispose [me]
           (.stop me))
  (stop [me]
    (log/info "apache io monitor stopping...")
    (c/cancelTimerTask (:ttask @me))
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
   (c/mutable<> FilePickerPluglet
                {:pspec (update-in spec
                                   [:conf] b/expandVarsInForm)
                 :parent _
                 :emAlias id })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

