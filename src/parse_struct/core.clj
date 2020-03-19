(ns parse-struct.core
  (:require [parse_struct.serialize]
            [parse_struct.deserialize]
            [parse_struct.utils]))

(defn deserialize parse_struct.deserialize/deserialize)
(defn type-size parse_struct.utils/type-size)

(defn serialize parse-struct.serialize/serialize)
