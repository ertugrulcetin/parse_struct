(ns base
  (:require [clojure.test :refer :all]
            [parse_struct.common-types :refer :all]
            [parse_struct.core :refer [parse-type type-size pows2]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [popen :refer :all])
  (:import (java.nio.file Files Path)))

(defn rand-range [s e]
  (+ (long (rand (- e s))) s))

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
  (apply str (repeatedly 10 #(rand-nth "qwertyuiopasdghklzxcvbnm1234567890"))))

(defn pad-nulls [s n]
  (apply str (take n (concat s (repeat (char 0))))))

(defn gen-val [name]
  ({"i8"   (i 8)
    "u8"   (u 8)
    "i16"  (i 16)
    "u16"  (u 16)
    "i32"  (i 32)
    "u32"  (u 32)
    "name8" (pad-nulls (gen-name (rand-int 8))
                       8)}
   name))

(def prim-generators {:int {true {1 #(i 8)
                                  2 #(i 16)
                                  4 #(i 32)}
                          false  {1  #(u 8)
                                  2 #(u 16)
                                  4 #(u 32)}}
                 :string   (partial #(pad-nulls (gen-name (rand-int %))
                                              %))})

(defn map-map [f m]
  (into {}
        (map (fn [[k v]]
               [k (f v)])
             m)))

(defn gen-struct [spec]
  (case (spec :type)
    :int ((get-in prim-generators [:int (spec :signed) (spec :bytes)]))
    :string ((prim-generators :string) (spec :bytes))
    :array (for [_ (range (spec :len))]
             (gen-struct (spec :element)))
    :struct (into {}
                  (map-map gen-struct (spec :definition)))))

(def prims {"i8"    i8
            "u8"    u8
            "i16"   i16
            "u16"   u16
            "i32"   i32
            "u32"   u32
            "name8" name8})
(defn gen-struct-spec [max-children levels]
  (let [total-count  (rand-int max-children)
        nested-count (if (pos-int? levels)
                       (rand-int total-count)
                       0)
        prim-count   (- total-count nested-count)
        array-count (rand-int nested-count)
        struct-count (- nested-count array-count)]
    (if (= total-count 1)
      (cond
        (and (pos-int? levels)
             (pos-int? array-count)) {:type    :array
                                      :len     (rand-int (max-children))
                                      :element (gen-struct-spec max-children (dec levels))}
        (and (pos-int? levels)
             (pos-int? prim-count)) {:type       :struct
                                     :definition {:nested (gen-struct-spec max-children (dec levels))}}
        :else (rand-nth (vals prims)))
      {:type       :struct
       :definition (reduce
                     (fn [res f]
                       (f res))
                     {}
                     [(fn [res]
                        (loop [res res
                               left prim-count]
                          (if (zero? left)
                            res
                            (let [[name type] (rand-nth (seq prims))]
                              (recur (assoc res (str name "-" (uuid)) type)
                                     (dec left))))))
                      (fn [res]
                        (loop [res res
                               left array-count]
                          (if (zero? left)
                            res
                            (recur (assoc res
                                     (str "array-" (uuid))
                                     {:type    :array
                                      :len     (rand-int max-children)
                                      :element (gen-struct-spec max-children (dec levels))})
                                   (dec left)))))
                      (fn [res]
                        (loop [res res
                               left struct-count]
                          (if (zero? left)
                            res
                            (recur (assoc res
                                     (str "struct-" (uuid))
                                     (gen-struct-spec max-children (dec levels)))
                                   (dec left)))))])})))

(defn unit-work []
  (let [dump-spec (gen-struct-spec 5 2)
        spec-json (json/write-str dump-spec)
        dump (gen-struct dump-spec)
        dump-json (json/write-str dump)]
    (let [cargo (popen ["cargo" "build" "--release"] :dir "test/dump-generator")]
      (if (not (zero? (exit-code cargo)))
        (println "cargo failed, fix dumper")
        (let [dumper (popen ["dump-generator" spec-json dump-json] :dir "test/dump-generator/target/debug")]
          (if (not (zero? (exit-code dumper)))
            (println "dumper failed")
            (let [dump (vec (stdout dumper))
                  parsed (parse-type dump-spec dump)]
              (when (not= parsed dump)
                (println "a dump failed: ")
                (println dump-spec)
                (println dump)))))))))

(defn -main []
  (doseq [_ (range 1)]
    (unit-work)))
