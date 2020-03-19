(ns parse-struct.serialize
  (:require [parse_struct.utils :refer [split-n take-exactly pows2 bitCount pow in-range zip-colls]])
  (:import (java.nio ByteBuffer ByteOrder)))

;;;
(defmacro make-int-writer [size]
  (let [min_range ({1 Byte/MIN_VALUE
                    2 Short/MIN_VALUE
                    4 Integer/MIN_VALUE
                    8 Long/MIN_VALUE} size)
        max_range ({1 Byte/MAX_VALUE
                    2 Short/MAX_VALUE
                    4 Integer/MAX_VALUE
                    8 Long/MAX_VALUE} size)
        putter ({1 '.put
                 2 '.putShort
                 4 '.putInt
                 8 '.putLong} size)
        caster ({1 byte
                 2 short
                 4 int
                 8 long} size)
        max_unsigned (pow 2 (* 8 size))
        signed?-arg (gensym)
        value-arg (gensym)
        sign-handled-var (gensym)
        bb-var (gensym)]
    `(fn [~signed?-arg ~value-arg]
       (let [~sign-handled-var (if (and (not ~signed?-arg)
                                        (not (<= 0 ~value-arg ~max_range)))
                                 (- ~value-arg ~max_unsigned)
                                 ~value-arg)]
         (if (not (<= ~min_range ~sign-handled-var ~max_range))
           (throw (new IllegalArgumentException (str "number: " ~value-arg " is out of range")))
           (let [~bb-var (ByteBuffer/allocate ~size)]
             (.order ~bb-var ByteOrder/LITTLE_ENDIAN)
             (~putter ~bb-var (~caster ~sign-handled-var))
             (.array ~bb-var)))))))

(def int-writers {1 (make-int-writer 1)
                  2 (make-int-writer 2)
                  4 (make-int-writer 4)
                  8 (make-int-writer 8)})
(defn int->bytes [{bc :bytes signed? :signed :as spec} value]
  ((int-writers bc) signed? value))

;;;
(defmacro make-float-writer [size]
  (let [putter ({4 '.putFloat
                 8 '.putDouble} size)
        val-arg (gensym)
        bb-arg (gensym)]
   `(fn [~val-arg]
     (let [~bb-arg (ByteBuffer/allocate size)]
       (.order ~bb-arg ByteOrder/LITTLE_ENDIAN)
       (~putter ~bb-arg ~val-arg)))))
(def float-writers {4 (make-float-writer 4)
                    8 (make-float-writer 8)})
(defn float->bytes [{bc :bytes} value]
  ((float-writers bc) value))

;;;
(defn string->bytes [{bc :bytes} value]
  (if (>= (count value) bc)
    (throw (new IllegalArgumentException (str "string: \"" value "\" is longer than the allotted space (" bc " bytes)")))
    (if (not (every? #(<= 0 (int %) 127) value))
      (throw (new IllegalArgumentException (str "string: \"" value "\" is not ascii")))
      (concat (.getBytes value) (repeat (- bc (count value)) (char 0))))))

(declare serialize)

(defn struct->bytes [{items :definition} value]
  (if (not (= (map first
                   (filter (fn [[_ spec]]
                             (not= :padding (spec :type)))
                           items))
              (keys value)))
    (throw (new IllegalArgumentException value))
    (apply concat (for [[name spec] items]
                    (serialize spec (value name))))))
(defn array->bytes [{len :len element :element} value]
  (apply concat (map #(serialize element %) (take-exactly len value))))

(defn serializer [type]
  (case type
    :int int->bytes
    :float float->bytes
    :string string->bytes
    :struct struct->bytes
    :array array->bytes
    :padding (fn [{bc :bytes} _]
               (repeat bc (byte 0)))
    (throw (new IllegalArgumentException "unknown :type"))))

(defn serialize [spec data]
  ((serializer (spec :type)) spec data))
