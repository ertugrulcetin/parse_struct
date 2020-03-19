(ns deserializing_test
  (:require [clojure.test :refer :all]
            [parse_struct.core :refer [parse type-size]]
            [parse_struct.utils :refer [pow bitCount read-file]]
            [parse_struct.common-types :refer :all]
            [clojure.data.json :as json]
            [popen :refer :all]
            [pjstadig.humane-test-output :as hto]))

(def dump1_def {:type       :struct
                :definition [[:a i8]
                             [:b u8]
                             [:c i16],
                             [:d u16],
                             [:e i32],
                             [:f u32],
                             [:g name8],
                             [:h name8]]})

(def dump1_data {:a -100,
                 :b 200,
                 :c -32000,
                 :d 33000,
                 :e -2100000000,
                 :f 2200000000,
                 :g "name",
                 :h "namefull"})

(def dump2_def {:type    :array
                :len     20
                :element dump1_def})

(def dump3_def {:type       :struct
                :definition [[:a i32]
                             [:c {:type       :string
                                  :bytes      6
                                  :trim_nulls true}]]})

(def dump3_data {:a 3000
                 :c "myname"})

(def dump4_def {:type    :array
                :len     10
                :element i32})

(def dump4_data (repeat 10 450))

(def dump5_def {:type    :array
                :len     20
                :element {:type    :array
                          :len     10
                          :element i32}})

(def dump5_data (repeat 20
                        (repeat 10 5)))

(def dump6_def {:type       :struct
                :definition [[:a i32]
                             [:b {:type       :struct
                                  :definition [[:a i32]
                                               [:c {:type       :string
                                                    :bytes      6
                                                    :trim_nulls false}]]}]]})

(def dump6_data {:a -45
                 :b {:a 0
                     :c (str "here" (new String (byte-array [0 0])))}})

(def dump7_def {:type :struct
                :definition [[:a u8]
                             [:b {:type :array
                                  :len 3
                                  :element dump3_def}]]})

(def dump7_data {:a 200
                 :b (repeat 3
                            {:a -5
                             :c "anothe"})})

(def dump8_def {:type       :struct
                :definition {:a i64
                             :b {:type    :array
                                 :len     3
                                 :element u64}}})

(def dump8_data {:a -6472394858488348972
                 :b (repeat 3 9823372036854775807)})

(defn -main []
  (hto/activate!)
  (exit-code (popen ["sh" "-c" "rm test/data/*"]))
  (let [rustc (popen ["rustc" "test/structs1.rs" "-o" "test/data/structs1"] :redirect true)]
    (if (not (zero? (exit-code rustc)))
      (do
        (println "rustc failed")
        (println (stdout rustc)))
      (if (not (zero? (exit-code (popen ["./test/data/structs1"]))))
        (println "dumper failed")
        (do
          (testing "dump 1"
            (let [bs (read-file "test/data/dmp1")
                  parsed (parse dump1_def bs)]
              (is (= parsed dump1_data))))
          (testing "dump 2"
            (let [bs (read-file "test/data/dmp2")
                  parsed (parse dump2_def bs)]
              (doseq [e parsed]
                (is (= e dump1_data)))))
          (testing "dump 3"
            (let [bs (read-file "test/data/dmp3")
                  parsed (parse dump3_def bs)]
              (is (= parsed dump3_data))))
          (testing "dump 4"
            (let [bs (read-file "test/data/dmp4")
                  parsed (parse dump4_def bs)]
              (is (= parsed dump4_data))))
          (testing "dump 5"
            (let [bs (read-file "test/data/dmp5")
                  parsed (parse dump5_def bs)]
              (is (= parsed dump5_data))))
          (testing "dump 6"
            (let [bs (read-file "test/data/dmp6")
                  parsed (parse dump6_def bs)]
              (is (= parsed dump6_data))))
          (testing "dump 7"
            (let [bs (read-file "test/data/dmp7")
                  parsed (parse dump7_def bs)]
              (is (= parsed dump7_data))))
          (testing "dump 8"
            (let [bs (read-file "test/data/dmp8")
                  parsed (parse dump8_def bs)]
              (is (= parsed dump8_data)))))))))
