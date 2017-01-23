;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Implementation for FilePicker."
      :author "Kenneth Leung"}

  czlab.wabbit.pugs.io.files

  (:require [czlab.basal.io :refer [mkdirs]]
            [czlab.basal.logging :as log]
            [clojure.java.io :as io])

  (:use [czlab.wabbit.pugs.io.loops]
        [czlab.wabbit.pugs.io.core]
        [czlab.wabbit.base.core]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.io FileFilter File IOException]
           [java.util Properties ResourceBundle]
           [czlab.wabbit.ctl Puglet Pluggable]
           [clojure.lang APersistentMap]
           [czlab.wabbit.pugs.io FileMsg]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- evt<>
  ""
  [co {:keys [fname fp]}]
  (let
    [eeid (str "file#" (seqint2))
     f (io/file fp)]
    (with-meta
      (reify FileMsg
        (checkAuthenticity [_] false)
        (originalFileName [_] fname)
        (source [_] co)
        (file [_] f)
        (id [_] eeid))
      {:typeid ::FileMsg})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- postPoll
  "Only look for new files"
  [co {:keys [recvFolder]} ^File f action]
  (let
    [orig (.getName f)]
    (if-some
      [cf (if (and (not= action :FP-DELETED)
                   (some? recvFolder))
            (try!
              (doto->> (io/file recvFolder orig)
                       (FileUtils/moveFile f))))]
      (->> (evt<> co {:fname orig
                      :fp cf
                      :action action})
           (dispatch! )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- toFMask
  ""
  ^FileFilter
  [^String mask]
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
(defn- init
  ""
  [conf cfg0]
  (let
    [{:keys [recvFolder
             fmask
             targetFolder] :as c2}
     (merge conf cfg0)
     root (expandVars targetFolder)
     dest (expandVars recvFolder)
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
  [co {:keys [targetFolder
              intervalSecs
              ^FileFilter fmask] :as cfg}]
  (let
    [obs (FileAlterationObserver. (io/file targetFolder) fmask)
     mon (-> (s2ms intervalSecs)
             (FileAlterationMonitor.))]
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
   :conf {:targetFolder "/home/dropbox"
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
(defn FilePicker
  ""
  ^Pluggable
  ([co] (FilePicker co (FilePickerSpec)))
  ([co {:keys [conf] :as spec}]
   (let
     [mee (keyword (juid))
      impl (muble<>)
      schedule
      #(let [_ %]
         (log/info "apache io monitor starting...")
         (some-> ^FileAlterationMonitor
                 (.getv impl mee) (.start)))
      par (threadedTimer {:schedule schedule})]
     (reify Pluggable
       (spec [_] specdef)
       (init [_ arg]
         (.copyEx impl (init conf arg)))
       (config [_] (.intern impl))
       (start [_ _]
         (let [m (fileMon<> co (.intern impl))]
           (.setv impl mee m)
           ((:start par) (.intern impl))))
       (stop [_]
         (log/info "apache io monitor stopping...")
         (some-> ^FileAlterationMonitor
                 (.getv impl mee) (.stop))
         (.unsetv impl mee))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


