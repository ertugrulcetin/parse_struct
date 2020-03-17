(ns parse_struct.core
  (:require [parse_struct.utils :refer [split-n take-exactly]])
  (:import (java.nio ByteBuffer ByteOrder)))

(defn type-size [sd]
  (case (:type sd)
    :array (* (sd :len)
              (type-size (sd :element)))
    :struct (reduce + (map type-size (vals (sd :definition))))
    :bool 1
    :byte 1
    (sd :bytes)))

(defn parseInt32 [bytes]
  (.getInt (.order (ByteBuffer/wrap (byte-array bytes)) ByteOrder/LITTLE_ENDIAN)))

(defn parseInt16 [bytes]
  (.getShort (.order (ByteBuffer/wrap (byte-array bytes)) ByteOrder/LITTLE_ENDIAN)))

(def pows2 {8 256
            16 65536
            32 4294967296})

(defn bitCount [num]
  (cond
    (instance? Integer num) 32
    (instance? Short num) 16))

(defn make-unsigned
  "16 and 32 bit ints"
  [num]
  (if (< num 0)
    (+ (long num) (pows2 (bitCount num)))
    num))

(def intParsers {2 parseInt16
                 4 parseInt32})

(defn parse-int [{bc :bytes signed? :signed} data]
  (let [parser (intParsers bc)
        parsedInt (parser (take-exactly bc data))
        signHandled (if (not signed?)
                      (make-unsigned parsedInt)
                      parsedInt)]
    signHandled))

(defn parse-string [{bc :bytes trim_nulls? :trim_nulls} data]
  (let [chunk (take-exactly bc data)
        trimmed (if (not= trim_nulls? false)
                  (take-while #(not= 0 %) chunk)
                  chunk)]
    (new String (byte-array trimmed))))

(defn parse-boolean [_ bytes]
  (not= 0 (first bytes)))

(defn parse-byte [{signed :signed} bytes]
  (let [byte (first (take-exactly 1 bytes))]
    (if signed
      byte
      (mod byte 256))))

(declare parse-struct)
(declare parse-array)
(declare parse-type)
(declare parsers)

(defn parse-array [{ed :element n :len} data]
  (map
    (partial parse-type ed)
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
            val (parse-type spec curr_chunk)]
        (recur (assoc res name val)
               (rest items_left)
               next_data_left)))))

(defn parse-type [spec data]
  ((parsers (spec :type)) spec data))

(def parsers {:byte   parse-byte
              :int    parse-int
              :string parse-string
              :bool   parse-boolean
              :array  parse-array
              :struct parse-struct})
