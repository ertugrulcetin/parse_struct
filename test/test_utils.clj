(ns test_utils
  (:import (java.nio.file Files Path)))


(defn read-file [fl]
  (Files/readAllBytes (Path/of fl (make-array String 0))))


