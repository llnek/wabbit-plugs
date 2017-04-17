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

(defobject FileMsg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ""
  [co {:keys [fname fp]}]

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
            (try!
              (doto->> (io/file recvFolder orig)
                       (FileUtils/moveFile f))))]
      (-> (evt<> (.parent ^Hierarchial plug)
                 {:fname orig
                  :fp cf :action action})
          dispatch! ))))

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
  [plug {:keys [targetFolder
                intervalSecs
                ^FileFilter fmask] :as cfg}]
  (let
    [obs (FileAlterationObserver. (io/file targetFolder) fmask)
     mon (-> (s2ms intervalSecs)
             FileAlterationMonitor.)]
    (->>
      (proxy [FileAlterationListenerAdaptor][]
        (onFileCreate [f]
          (postPoll co cfg f :FP-CREATED))
        (onFileChange [f]
          (postPoll co cfg f :FP-CHANGED))
        (onFileDelete [f]
          (postPoll co cfg f :FP-DELETED)))
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
(defn FilePicker "" ^czlab.wabbit.xpis.Pluglet

  ([_ id] (FilePicker _ id (FilePickerSpec)))
  ([_ id spec]
   (let
     [pspec (update-in spec
                       [:conf] expandVarsInForm)
      vtbl
      {:schedule (fn [vt me]
                   (log/info "apache io monitor starting...")
                   (some-> ^FileAlterationMonitor (:mon @me) .start))
       :init (fn [vt me arg]
               (let [c (get-in @me [:pspec :conf])]
                 (alterStateful
                   me update-in [:conf] (prevarCfg (init2 c arg)))))
       :start (fn [vt me _]
                (->> (.config ^Config me)
                     (fileMon<> me)
                     (alterStateful me assoc :mon))
                (rvtbl' vt :start _))
       :stop (fn [vt me]
               (let [m (:mon @me)]
                 (alterStateful me dissoc :mon)
                 (log/info "apache io monitor stopping...")
                 (some-> ^FileAlterationMonitor m .stop)))}]

     (pluglet<> pspec
                (svtbl vtbl
                       (threadedVtbl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

