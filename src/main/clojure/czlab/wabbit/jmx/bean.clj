;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.jmx.bean

  (:require [czlab.basal.log :as log]
            [clojure.string :as cs]
            [czlab.basal.meta :as m]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s])

  (:import [java.lang Exception IllegalArgumentException]
           [java.lang.reflect Field Method]
           [java.util Arrays]
           [javax.management
            AttributeList
            Attribute
            DynamicMBean
            MBeanException
            MBeanInfo
            MBeanAttributeInfo
            MBeanOperationInfo
            MBeanParameterInfo
            ReflectionException
            AttributeNotFoundException]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(c/decl-object NameParams)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- toStringNameParams "" [nps]
  (let [{:keys [name params]} nps]
    (if (empty? params)
      name
      (str name "/" (cs/join "#" params)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nameParams<> ""
  ([name] (nameParams<> name nil))
  ([name pms]
   (c/object<> NameParams
               {:name name :params (or pms [])})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(c/decl-object BFieldInfo)
(c/decl-object BPropInfo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkBFieldInfo
  ""
  [field getter? setter?]
  (c/object<> BFieldInfo
              {:getter? getter?
               :setter? setter?
               :field field}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getPropType "" [prop]
  (if-some [g (:getter prop)]
    (.getReturnType ^Method g)
    (let [ps (some-> ^Method
                     (:setter prop)
                     .getParameterTypes)]
      (if (== 1 (count ps)) (first ps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- isPropQuery? "" [prop]
  (if-some [^Method g (:getter prop)]
    (and (-> (.getName g)
             (.startsWith "is"))
         (m/isBoolean? (.getReturnType g))) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkBPropInfo "" [prop desc getr setr]
  (c/object<> BPropInfo
              {:desc desc :name prop :getter getr :setter setr }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwUnknownError
  "" [attr]
  (c/trap! AttributeNotFoundException
           (format "Unknown property %s" attr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwBeanError
  "" [^String msg]
  (c/trap! MBeanException (c/exp! Exception msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- assertArgs
  "" [mtd ptypes n]
  (if (not= n (count ptypes))
    (c/throwBadArg (str "\"" mtd "\" needs " n "args"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeGetPropName
  "" ^String [^String mn]
  (let
    [pos (cond
           (or (.startsWith mn "get")
               (.startsWith mn "set")) 3
           (.startsWith mn "is") 2
           :else -1)]
    (if (< pos 0)
      ""
      (str (Character/toLowerCase (.charAt mn pos))
           (.substring mn (+ pos 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkParameterInfo ""
  [^Method mtd]
  (c/preduce<vec>
    #(let [[^Class t n] %2]
       (->> (MBeanParameterInfo.
              (format "p%d" n) (.getName t) "")
            (conj! %1)))
    (partition
      2
      (interleave
        (vec (.getParameterTypes mtd))
        (map inc (range))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- testJmxType
  "if primitive types"
  ^Class [cz]
  (if (or (m/isBoolean? cz)
          (m/isVoid? cz)
          (m/isObject? cz)
          (m/isString? cz)
          (m/isShort? cz)
          (m/isLong? cz)
          (m/isInt? cz)
          (m/isDouble? cz)
          (m/isFloat? cz)
          (m/isChar? cz))
    cz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- testJmxTypes?
  "Make sure we are dealing with primitive types"
  [^Class rtype ptypes]
  (if (some #(nil? (testJmxType %))(seq ptypes))
    false
    (some? (testJmxType rtype))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- beanAttrInfo<> ""
  ^MBeanAttributeInfo
  [prop]
  (MBeanAttributeInfo. (:name prop)
                       (str (some-> ^Class
                                    (getPropType prop) .getName))
                       (:desc prop)
                       (some? (:getter prop))
                       (some? (:setter prop))
                       (isPropQuery? prop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- beanFieldInfo<> ""
  [^Field f]
  (let [fnm (.getName f)
        t (.getType f)]
    (MBeanAttributeInfo. fnm
                         (.getName t)
                         (str fnm " attribute")
                         true
                         true
                         (and (.startsWith fnm "is")
                              (m/isBoolean? t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- beanOpInfo<> ""
  [^Method m]
  (let [t (.getReturnType m)
        mn (.getName m)]
    (MBeanOperationInfo. mn
                         (str mn " operation")
                         (->> (mkParameterInfo m)
                              (c/vargs MBeanParameterInfo))
                         (.getName t)
                         MBeanOperationInfo/ACTION_INFO)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleProps2
  "Only deal with getters and setters"
  [^Method mtd propsBin]
  (let
    [ptypes (.getParameterTypes mtd)
     rtype (.getReturnType mtd)
     mn (.getName mtd)
     pname (maybeGetPropName mn)
     methodInfo (propsBin pname)]
    (cond
      (s/nichts? pname)
      propsBin

      (and (s/swAny? mn ["get" "is"])
           (empty? ptypes))
      (if (nil? methodInfo)
        (->> (mkBPropInfo pname "" mtd nil)
             (assoc! propsBin pname))
        (->> (assoc methodInfo :getter mtd)
             (assoc! propsBin pname)))

      (and (.startsWith mn "set")
           (== 1 (count ptypes)))
      (if (nil? methodInfo)
        (->> (mkBPropInfo pname "" nil mtd)
             (assoc! propsBin pname))
        (->> (assoc methodInfo :setter mtd)
             (assoc! propsBin pname)))

      :else propsBin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleProps "" [cz]
  (let
    [props
     (c/preduce<map>
       #(handleProps2 %2 %1) (.getMethods ^Class cz))
     ba
     (c/preduce<vec>
       #(let [[k v] %2]
          (if-some [mt (testJmxType
                         (getPropType v))]
            (conj! %1 (beanAttrInfo<> v)) %1)) props)]
    [ba props]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFlds "" [cz]
  (let
    [dcls (.getDeclaredFields ^Class cz)
     flds
     (c/preduce<map>
       #(let [^Field f %2
              fnm (.getName f)]
          (if (.isAccessible f)
            (->> (mkBFieldInfo f true true) (assoc! %1 fnm)) %1)) dcls)
     rc
     (c/preduce<vec>
       #(let [^Field f %2]
          (if-not (.isAccessible f)
            (conj! %1 (beanFieldInfo<> f)) %1)) dcls)]
    [rc flds]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleMethods2
  "" [^Method m mtds rc]
  (let
    [ptypes (.getParameterTypes m)
     rtype (.getReturnType m)
     mn (.getName m)]
    (if (some? (testJmxTypes? rtype ptypes))
      [(assoc! mtds
               (->> (mapv (fn [^Class c]
                            (.getName c))
                          ptypes)
                    (nameParams<> mn)) m)
       (conj! rc (beanOpInfo<> m))]
      [mtds rc])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleMethods "" [cz]
  (log/info "jmx-bean: processing methods for class: %s" cz)
  (loop
    [ms (.getMethods ^Class cz)
     mtds (transient {})
     rc (transient [])]
    (if (empty? ms)
      [(c/pcoll! rc) (c/pcoll! mtds)]
      (let [[m r]
            (handleMethods2 (first ms) mtds rc)]
        (recur (rest ms) m r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jmxBean<>
  "Make a JMX bean from this object"
  ^DynamicMBean
  [^Object obj]
  {:pre [(some? obj)]}
  (let
    [cz (.getClass obj)
     [ps propsMap] (handleProps cz)
     [fs fldsMap] (handleFlds cz)
     [ms mtdsMap] (handleMethods cz)
     bi (MBeanInfo.
          (.getName cz)
          (str "About: " cz)
          (->> (concat ps fs)
               (c/vargs MBeanAttributeInfo))
          nil
          (c/vargs MBeanOperationInfo ms)
          nil)]
    (reify DynamicMBean
      (getAttribute [_ attr]
        (let [prop (propsMap attr)
              fld (fldsMap attr)]
          (cond
            (nil? prop)
            (do
              (if (or (nil? fld)
                      (not (:getter? fld)))
                (throwUnknownError attr))
              (.get ^Field (:field fld) obj))

            (nil? (:getter prop))
            (throwUnknownError attr)

            :else
            (-> ^Method
                (:getter prop)
                (.invoke obj (object-array 0))))))

      (getAttributes [this attrs]
        (c/do-with [rcl (AttributeList.)]
          (doseq [^String nm (seq attrs)]
            (try
              (->> (.getAttribute this nm)
                   (Attribute. nm)
                   (.add rcl))
              (catch Throwable e#
                (log/exception e#)
                (->> (.getMessage e#)
                     (Attribute. nm) (.add rcl)))))))

      (getMBeanInfo [_] bi)

      (setAttribute [_ attr]
        (let [v (.getValue attr)
              an (.getName attr)
              prop (propsMap an)
              fld (fldsMap an)]
          (cond
            (nil? prop)
            (do
              (if (or (nil? fld)
                      (not (:setter? fld)))
                (throwUnknownError an))
              (.set ^Field (:field fld) obj v))

            (nil? (:setter prop))
            (throwUnknownError an)

            :else
            (-> ^Method (:setter prop) (.invoke obj v)))))

      (setAttributes [this attrs]
        (c/do-with [rcl (AttributeList. (count attrs))]
          (doseq [^Attribute a (seq attrs)
                 :let [nn (.getName a)]]
            (try
              (.setAttribute this a)
              (->> (.getAttribute this nn)
                   (Attribute. nn)
                   (.add rcl))
              (catch Throwable e#
                (log/exception e#)
                (->> (.getMessage e#)
                     (Attribute. nn) (.add rcl)))))))

      (invoke [_ opName params sig]
        (if-some
          [^Method mtd (mtdsMap (nameParams<>
                                  opName (into [] sig)))]
          (c/try!
            (log/debug "jmx-invoke: '%s'\n%s%s\n%s%s"
                       opName "(params) "
                       (seq params) "(sig) " (seq sig))
            (if (empty? params)
              (.invoke mtd obj (object-array 0))
              (.invoke mtd obj params)))
          (throwBeanError
            (format "Unknown operation '%s'" opName)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

