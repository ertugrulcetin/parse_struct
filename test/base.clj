(ns base
  (:require [clojure.test :refer :all]
            [parse_struct.common-types :refer :all]
            [parse_struct.core :refer [parse-type type-size pows2]]
            [clojure.data.json :as json])
  (:import (java.nio.file Files Path)))

(def dump-struct {:type       :struct
                  :definition {:b     i8
                               :ub    u8
                               :s     i16
                               :us    u16
                               :i     i32
                               :ui    u32
                               :name1 name8
                               :name2 name8}})

(defn rand-range [s e]
  (+ (rand-int (- e s)) s))

(defn half [n]
  (/ n 2))

(defn neg [n]
  (* -1 n))

(defn i [bits]
  (rand-range (neg (half (pows2 bits)))
              (half (pows2 bits))))

(defn u [bits]
  (rand-range 0
              (pows2 bits)))

(def max_char (inc (int (Character/MAX_VALUE))))

(defn gen-name [n]
  (for [_ (range n)]
    (char (rand-int max_char))))

(defn uuid []
  (apply str (repeatedly 20 #(rand-nth "qwertyuiopasdghklzxcvbnm1234567890"))))

(defn pad-nulls [s n]
  (apply str (take n (concat s (repeat (char 0))))))

(defn gen-struct []
  {"i8"   (i 8)
   "u8"   (u 8)
   "i16"  (i 16)
   "u16"  (u 16)
   "i32"  (i 32)
   "u32"  (u 32)
   "name" (pad-nulls (gen-name (rand-int 8))
                     8)})

(def prims {"i8"    i8
            "u8"    u8
            "i16"   i16
            "u16"   u16
            "i32"   i32
            "u32"   u32
            "name8" name8})
(defn gen-struct-def [levels]
  (let [total-count  (rand-int 5)
        nested-count (if (pos-int? levels)
                       (rand-int total-count)
                       0)
        prim-count   (- total-count nested-count)
        array-count (rand-int nested-count)
        struct-count (- nested-count array-count)]
    (reduce
      (fn [res f]
        (f res))
      {}
      [(fn [res]
         (loop [res res
                left prim-count]
           (if (zero? left)
             res
             (let [[name type] (rand-nth (seq prims))]
               (recur (assoc res name type)
                      (dec left))))))
       (fn [res]
         (loop [res res
                left array-count]
           (if (zero? left)
             res
             (recur (assoc res
                      (str "array-" (uuid))
                      {:type :array
                       :element (gen-struct-def (dec levels))})
                    (dec left)))))
       (fn [res]
         (loop [res res
                left struct-count]
           (if (zero? left)
             res
             (recur (assoc res
                      (str "struct-" (uuid))
                      {:type :strut
                       :definition (gen-struct-def (dec levels))})
                    (dec left)))))])))

(defn main []
  (doseq [_ (range 1)]
    (let [dump (gen-struct)
          dump-json (json/write-str dump)]
      )))

(defn -main [& args]
  (let [bs (vec (Files/readAllBytes (Path/of "test/dmp" (make-array String 0))))
        parsed (parse-type dump-struct (subvec bs 2))]
    (testing "short"
      (is (= -32000 (parsed :s))))
    (testing "unsigned short"
      (is (= 33000 (parsed :us))))))