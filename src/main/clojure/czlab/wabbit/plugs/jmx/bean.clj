;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.wabbit.plugs.jmx.bean

  (:require [czlab.basal.logging :as log])

  (:use [czlab.basal.meta]
        [czlab.basal.core]
        [czlab.basal.str])

  (:import [java.lang Exception IllegalArgumentException]
           [java.lang.reflect Field Method]
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
            AttributeNotFoundException]
           [java.util Arrays]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)

(decl-mutable NameParams
  Object
  (toString [me]
    (let [{:keys [name params]} @me]
      (if (empty? params)
        name
        (str name "/" (cs/join "#" params)))))
  (hashCode [me]
    (let [{:keys [name params]} @me]
      (int (reduce
             #(+ %1 (.hashCode %2))
             (* 31 (+ 31 (.hashCode name)))
             params))))
  (equals [me obj]
    (and obj
         (= (.getClass me) (.getClass obj))
         (= (:name @me) (:name @obj))
         (= (:params @me) (:params @obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn nameParams<> ""
  ([name] (nameParams<> name nil))
  ([name pms]
   (mutable<> NameParams
              {:name name :params (or pms [])})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol ^:private BPropInfo
  ""
  (set-setter [_ ^Method m] )
  (set-getter [_ ^Method m] )
  (^Method get-getter [_] )
  (^Method get-setter [_] )
  (^Class get-type [_] )
  (^String get-desc [_] )
  (^String get-name [_] )
  (^boolean is-query? [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defprotocol ^:private BFieldInfo
  ""
  (^boolean is-getter? [_] )
  (^boolean is-setter? [_] )
  (^Field get-field [_] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkBFieldInfo
  ""
  [^Field fld getr? setr?]
  (reify BFieldInfo
    (is-getter? [_] getr?)
    (is-setter? [_] setr?)
    (get-field [_] fld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(decl-mutable BPropInfoObj
  BPropInfo
  (get-type [me]
    (if-some [g (get-getter me)]
      (.getReturnType g)
      (let [ps (some-> (get-setter me)
                       .getParameterTypes)]
        (if (== 1 (count ps)) (first ps)))))
  (set-setter [me m] (setf! me :setr m))
  (set-getter [me m] (setf! me :getr m))
  (get-setter [me] (:setr @me))
  (get-getter [me] (:getr @me))
  (get-desc [me] (:descn @me))
  (get-name [me] (:prop @me))
  (is-query? [me]
    (if-some [g (get-getter me)]
      (and (-> (.getName g)
               (.startsWith "is"))
           (isBoolean? (.getReturnType g)))
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mkBPropInfo "" [^String prop ^String descn
                       ^Method getr ^Method setr]
  (mutable<> BPropInfoObj
             {:descn descn
              :prop prop
              :getr getr
              :setr setr
              :type nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwUnknownError
  "" [attr]
  (trap! AttributeNotFoundException
         (format "Unknown property %s" attr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- throwBeanError
  "" [^String msg]
  (trap! MBeanException (exp! Exception msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- assertArgs
  "" [mtd ptypes n]
  (if (not= n (count ptypes))
    (throwBadArg (str "\"" mtd "\" needs " n "args"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeGetPropName ""
  ^String
  [^String mn]
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
  (preduce<vec>
    #(let [[^Class t n] %2]
       (->> (MBeanParameterInfo.
              (format "p%d" n)
              (.getName t)
              "")
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
  (if (or (isBoolean? cz)
          (isVoid? cz)
          (isObject? cz)
          (isString? cz)
          (isShort? cz)
          (isLong? cz)
          (isInt? cz)
          (isDouble? cz)
          (isFloat? cz)
          (isChar? cz))
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
  [v]
  (MBeanAttributeInfo. (get-name v)
                       (-> (get-type v)
                           .getName)
                       (get-desc v)
                       (some? (get-getter v))
                       (some? (get-setter v))
                       (is-query? v)))

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
                              (isBoolean? t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- beanOpInfo<> ""
  [^Method m]
  (let [t (.getReturnType m)
        mn (.getName m)]
    (MBeanOperationInfo. mn
                         (str mn " operation")
                         (->> (mkParameterInfo m)
                              (vargs MBeanParameterInfo))
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
      (nichts? pname)
      propsBin

      (and (swAny? mn ["get" "is"])
           (empty? ptypes))
      (if (nil? methodInfo)
        (->> (mkBPropInfo pname "" mtd nil)
             (assoc! propsBin pname))
        (do
          (set-getter methodInfo mtd)
          propsBin))

      (and (.startsWith mn "set")
           (== 1 (count ptypes)))
      (if (nil? methodInfo)
        (->> (mkBPropInfo pname "" nil mtd)
             (assoc! propsBin pname))
        (do
          (set-setter methodInfo mtd)
          propsBin))

      :else propsBin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleProps "" [cz]
  (let
    [props
     (preduce<map>
       #(handleProps2 %2 %1) (. ^Class cz getMethods))
     ba
     (preduce<vec>
       #(let [[k v] %2]
          (if-some
            [mt (testJmxType (get-type v))]
            (conj! %1 (beanAttrInfo<> v))
            %1))
       props)]
    [ba props]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleFlds "" [cz]
  (let
    [dcls (. ^Class cz getDeclaredFields)
     flds
     (preduce<map>
       #(let [^Field f %2
              fnm (.getName f)]
          (if (.isAccessible f)
            (->> (mkBFieldInfo f true true)
                 (assoc! %1 fnm))
            %1))
       dcls)
     rc
     (preduce<vec>
       #(let [^Field f %2]
          (if-not (.isAccessible f)
            (conj! %1 (beanFieldInfo<> f))
            %1))
       dcls)]
    [rc flds]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleMethods2
  "" [^Method m mtds rc]
  (let
    [ptypes (.getParameterTypes m)
     rtype (.getReturnType m)
     mn (.getName m)]
    (if (some? (testJmxTypes? rtype
                              ptypes))
      [(assoc! mtds
               (->> ptypes
                    (mapv #(.getName ^Class %))
                    (nameParams<> mn))
               m)
       (conj! rc (beanOpInfo<> m))]
      [mtds rc])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- handleMethods "" [cz]
  (log/info "jmx-bean: processing methods for class: %s" cz)
  (loop
    [ms (. ^Class cz getMethods)
     mtds (transient {})
     rc (transient [])]
    (if (empty? ms)
      [(pcoll! rc) (pcoll! mtds)]
      (let [[m r]
            (handleMethods2 (first ms) mtds rc)]
        (recur (rest ms) m r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn jmxBean<>
  "Make a JMX bean from this object"
  ^DynamicMBean
  [obj]
  {:pre [(some? obj)]}
  (let
    [cz (. ^Object obj getClass)
     [ps propsMap] (handleProps cz)
     [fs fldsMap] (handleFlds cz)
     [ms mtdsMap] (handleMethods cz)
     impl (muble<>)
     bi (MBeanInfo.
          (.getName cz)
          (str "About: " cz)
          (->> (concat ps fs)
               (vargs MBeanAttributeInfo))
          nil
          (vargs MBeanOperationInfo ms)
          nil)]
    (reify DynamicMBean
      (getAttribute [_ attr]
        (let [prop (propsMap attr)
              fld (fldsMap attr)]
          (cond
            (nil? prop)
            (do
              (if (or (nil? fld)
                      (not (is-getter? fld)))
                (throwUnknownError attr))
              (.get (get-field fld) obj))

            (nil? (get-getter prop))
            (throwUnknownError attr)

            :else
            (-> (get-getter prop)
                (.invoke obj (object-array 0))))))

      (getAttributes [this attrs]
        (let [rcl (AttributeList.)]
          (doseq [^String nm (seq attrs)]
            (try
              (->> (.getAttribute this nm)
                   (Attribute. nm)
                   (.add rcl))
              (catch Throwable e#
                (log/exception e#)
                (->> (.getMessage e#)
                     (Attribute. nm)
                     (.add rcl)))))
          rcl))

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
                      (not (is-setter? fld)))
                (throwUnknownError an))
              (.set (get-field fld) obj v))

            (nil? (get-setter prop))
            (throwUnknownError an)

            :else
            (-> (get-setter prop)
                (.invoke obj v)))))

      (setAttributes [this attrs]
        (let [rcl (AttributeList. (count attrs))]
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
                     (Attribute. nn)
                     (.add rcl)))))
          rcl))

      (invoke [_ opName params sig]
        (if-some
          [^Method mtd (mtdsMap (nameParams<> opName
                                              (into [] sig)))]
          (try!
            (log/debug "jmx-invoke: '%s'\n%s%s\n%s%s"
                       opName "(params) " (seq params) "(sig) " (seq sig))
            (if (empty? params)
              (.invoke mtd obj (object-array 0))
              (.invoke mtd obj params)))
          (throwBeanError
            (format "Unknown operation '%s'" opName)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

