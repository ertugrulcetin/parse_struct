(ns all-tests
  (:require [clojure.test :refer :all]
            [pjstadig.humane-test-output :as hto]
            [deserializing_test]
            [serializing_test]
            [roundtrip_test]))

(defn -main []
  (hto/activate!)
  (run-tests 'deserializing_test
             'serializing_test
             'roundtrip_test))
