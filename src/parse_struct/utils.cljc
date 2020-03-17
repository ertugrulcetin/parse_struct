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
