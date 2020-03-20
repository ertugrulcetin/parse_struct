(ns roundtrip-test
  (:require [clojure.test :refer :all]
            [parse_struct.common-types :refer :all]
            [parse_struct.utils :refer [pow type-size]]
            [parse_struct.deserialize :refer [deserialize]]
            [parse_struct.serialize :refer [serialize]]
            [clojure.string :as string]
            [popen :refer :all]
            [pjstadig.humane-test-output :as hto])
  (:import (java.nio.file Files Path)
           (clojure.lang APersistentMap IPersistentVector IPersistentMap PersistentQueue)))

(defn rand-range [s e]
  (+ (long (rand (- e s))) s))

(defn half [n]
  (/ n 2))

(defn neg [n]
  (* -1 n))

(defn i [bits]
  (let [signed-limit (pow 2 (dec bits))]
    (rand-range (neg signed-limit)
                signed-limit)))

(defn u [bits]
  (rand-range 0 (pow 2 bits)))

(def max_char (inc (int (Character/MAX_VALUE))))
(defn gen-name [n]
  (for [_ (range n)]
    (char (rand-int 128))))

(defn uuid []
  (apply str (repeatedly 10 #(rand-nth "qwertyuiopasdghklzxcvbnm1234567890"))))

(defn pad-nulls [s n]
  (apply str (take n (concat s (repeat (char 0))))))

(def prim-generators {:int    {true  {1 #(i 8)
                                      2 #(i 16)
                                      4 #(i 32)}
                               false {1 #(u 8)
                                      2 #(u 16)
                                      4 #(u 32)}}
                      :string {true  #(apply str (gen-name (rand-int %)))
                               false #(pad-nulls (gen-name (rand-int %))
                                                 %)}})

(defn gen-struct-val [spec]
  (case (spec :type)
    :int ((get-in prim-generators [:int (spec :signed) (spec :bytes)]))
    :string ((get-in prim-generators [:string (spec :trim_nulls)]) (spec :bytes))
    :array (for [_ (range (spec :len))]
             (gen-struct-val (spec :element)))
    :struct (into {}
                  (map (fn [[name value]] [name (gen-struct-val value)]) (spec :definition)))))

(def prims {"i8"    i8
            "u8"    u8
            "i16"   i16
            "u16"   u16
            "i32"   i32
            "u32"   u32
            "name8" name8})

(declare gen-rand-spec)

(defn gen-rand-prim-spec [_]
  (rand-nth (vals prims)))

(defn gen-rand-array-spec [{max-len :max-array-len :as characteristics}]
  {:type    :array
   :len     (rand-int max-len)
   :element (gen-rand-spec (update characteristics :max-depth dec))})

(defn gen-rand-struct-spec [{max-children :max-struct-children :as characteristics}]
  (let [count (rand-int max-children)
        next-characteristics (update characteristics :max-depth dec)]
    {:type       :struct
     :definition (map (fn [_] [(uuid) (gen-rand-spec next-characteristics)]) (range count))}))

(defn gen-rand-spec [{max-depth :max-depth :as characteristics}]
  (if (zero? max-depth)
    (gen-rand-prim-spec nil)
    ((rand-nth [gen-rand-prim-spec
                gen-rand-array-spec
                gen-rand-struct-spec]) characteristics)))

(defn unit-work [id]
  (testing (str "roundtrip number: " id)
    (let [spec (gen-rand-spec {:max-array-len       5
                               :max-struct-children 5
                               :max-depth           3})
          value (gen-struct-val spec)]
      (is (= value (deserialize spec (serialize spec value)))
          (let [spec_file (str "test/data/failed_spec_" id ".edn")
                value_file (str "test/data/failed_value_" id ".edn")]
            (clojure.pprint/pprint spec (clojure.java.io/writer spec_file))
            (clojure.pprint/pprint value (clojure.java.io/writer value_file))
            (str "serialize-deserialize roundtrip failed for dump number: " id "\n"
                 "spec saved to: " spec_file "\n"
                 "value saved to: " value_file))))))

(defn get-data []
  (def a (read-string (slurp "test/data/failed_spec_10.edn")))
  (def b (read-string (slurp "test/data/failed_value_10.edn"))))

(deftest roundtrip
  (doseq [id (range 100)]
    (unit-work id)))
