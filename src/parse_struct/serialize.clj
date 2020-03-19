(ns parse-struct.serialize
  (:require [parse_struct.utils :refer [split-n take-exactly pows2 bitCount pow]])
  (:import (java.nio ByteBuffer ByteOrder)))

(defn int->bytes [{bytes :bytes signed? :signed} data]
  (let [bb (ByteBuffer/allocate bytes)
        max_val (pow 2 (* 8 bytes))
        sign-handled (if (not signed?)
                       (- data max_val)
                       data)]
    (if (> sign-handled max_val)
      (throw (new IllegalArgumentException (str "Value: " data " too big to fit in " bytes " bytes")))
      (do
        (case bytes
          1 (.put bb sign-handled)
          2 (.putShort bb sign-handled)
          4 (.putInt bb sign-handled))
        (.array bb)))))

(defn string->bytes [])
(defn struct->bytes [])
(defn array->bytes [])

(defn serializer [type]
  (case type
    :int int->bytes
    :string string->bytes
    :struct struct->bytes
    :array array->bytes
    (throw (new IllegalArgumentException "unknown :type"))))

(defn serialize [spec data]
  ((serializer (spec :type)) spec data))
