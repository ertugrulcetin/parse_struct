(ns benchmark
  (:require [parse_struct.core :refer [serialize deserialize]]
            [roundtrip_test :refer [gen-struct-val]]))

(defn -main []
  (let [spec (read-string (slurp (str "test/big_spec.edn")))
        value (gen-struct-val spec)]
    (println "warming up")
    (doseq [i (range 1 4)]
      (println i)
      (count (deserialize spec (serialize spec value))))
    (println "done")
    (time (= (deserialize spec (serialize spec value))
             (seq value)))))
