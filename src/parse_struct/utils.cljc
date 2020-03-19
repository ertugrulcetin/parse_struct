(ns parse_struct.utils)

(defn split-n [n coll]
  (loop [fst (transient [])
         scnd coll
         left n]
    (if (zero? left)
      [(persistent! fst) scnd]
      (if (empty? scnd)
        (throw (new IndexOutOfBoundsException))
        (recur (conj! fst (first scnd))
               (rest scnd)
               (dec left))))))

(defn subvec-n [v s n]
  (subvec v s (+ s n)))

(defn take-exactly [n coll]
  (if (pos-int? n)
    (if (empty? coll)
      (throw (new Exception))
      (lazy-seq (cons (first coll)
                     (take-exactly (dec n)
                                   (rest coll)))))))

(def pows2 {8 256
            16 65536
            32 4294967296})

(defn pow [b e]
  (reduce
    (fn [r _] (* r b))
    1 (range e)))

(defn bitCount [num]
  (cond
    (instance? Integer num) 32
    (instance? Short num) 16
    (instance? Byte num) 8
    :else (throw (new IllegalStateException "unknown type"))))

(defn type-size [sd]
  (case (:type sd)
    :array (* (sd :len)
              (type-size (sd :element)))
    :struct (reduce + (map type-size (vals (sd :definition))))
    (sd :bytes)))