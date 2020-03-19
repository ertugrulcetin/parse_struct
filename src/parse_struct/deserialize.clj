(ns parse_struct.deserialize
  (:require [parse_struct.utils :refer [split-n take-exactly pow pows2 bitCount type-size zip-colls]]
            [clojure.spec.alpha :as s])
  (:import (java.nio ByteBuffer ByteOrder)))

(defn parseInt64 [bytes]
  (.getLong (.order (ByteBuffer/wrap (byte-array bytes)) ByteOrder/LITTLE_ENDIAN)))

(defn parseInt32 [bytes]
  (.getInt (.order (ByteBuffer/wrap (byte-array bytes)) ByteOrder/LITTLE_ENDIAN)))

(defn parseInt16 [bytes]
  (.getShort (.order (ByteBuffer/wrap (byte-array bytes)) ByteOrder/LITTLE_ENDIAN)))

(defn parseInt8 [bytes]
  (.get (ByteBuffer/wrap (byte-array bytes))))

(defmacro make-unsigned-maker [bits]
  (let [target-type (if (< bits 64)
                      long
                      bigint)
        offset (pows2 bits)
        numarg (gensym)]
    `(fn [~numarg]
       (if (< ~numarg 0)
         (+ (~target-type ~numarg) ~offset)
         ~numarg))))

(def intParsers {1 [parseInt8 (make-unsigned-maker 8)]
                 2 [parseInt16 (make-unsigned-maker 16)]
                 4 [parseInt32 (make-unsigned-maker 32)]
                 8 [parseInt64 (make-unsigned-maker 64)]})

(defn parse-int [{bc :bytes signed? :signed} data]
  (let [[parser sign-handler] (intParsers bc)
        parsedInt (parser (take-exactly bc data))
        signHandled (if (not signed?)
                      (sign-handler parsedInt)
                      parsedInt)]
    signHandled))

(defn parse-float [{bc :bytes} data]
  (let [bb (.order (ByteBuffer/wrap (byte-array (take-exactly bc data))) ByteOrder/LITTLE_ENDIAN)]
   (case bc
     4 (.getFloat bb)
     8 (.getDouble bb)
     (throw (new IllegalArgumentException "Floats can have 4 or 8 bytes")))))

(defn parse-string [{bc :bytes trim_nulls? :trim_nulls} data]
  (let [chunk (take-exactly bc data)
        trimmed (if (not= trim_nulls? false)
                  (take-while #(not= 0 %) chunk)
                  chunk)]
    (new String (byte-array trimmed) "ASCII")))

(declare parse-struct)
(declare parse-array)
(declare deserialize)
(declare parsers)

(defn parse-array [{ed :element n :len} data]
  (map
    (partial deserialize ed)
    (take-exactly n (partition (type-size ed) data))))

(defn parse-struct [{definition :definition} data]
  (loop [res {}
         items_left definition
         data_left data]
    (if (empty? items_left)
      res
      (let [[name spec] (first items_left)
            size (type-size spec)
            [curr_chunk next_data_left] (split-n size data_left)
            val (deserialize spec curr_chunk)]
        (recur (if (= (spec :type) :padding)
                 res
                 (assoc res name val))
               (rest items_left)
               next_data_left)))))

(defn deserialize [spec data]
  ((parsers (spec :type)) spec data))

(def parsers {:int    parse-int
              :float  parse-float
              :string parse-string
              :array  parse-array
              :struct parse-struct
              :padding (fn [_ _])})
