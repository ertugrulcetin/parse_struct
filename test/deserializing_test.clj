(ns deserializing_test
  (:require [clojure.test :refer :all]
            [parse_struct.core :refer [deserialize type-size]]
            [parse_struct.deserialize :refer [parse]]
            [test_utils :refer [read-file]]
            [dump_defs :refer :all]))

(deftest deserialization
  (doseq [i (range 1 9)
          :let [dump_file (str "test/data/dmp" i)
                dump_def (deref (ns-resolve 'dump_defs (symbol (str "dump" i "_def"))))
                dump_data (deref (ns-resolve 'dump_defs (symbol (str "dump" i "_data"))))]]
    (testing dump_file
      (let [bs (read-file dump_file)
            parsed (parse dump_def bs)]
        (is (= dump_data parsed))))))

