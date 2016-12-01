(ns advent-of-code.day4
  (:require digest))

(defn n-zeroes? [n md5]
  (= (repeat n \0)
     (take n md5)))

(defn hashes
  ([input] (hashes input 1))
  ([input n] (cons (digest/md5 (str input n))
                   (lazy-seq (hashes input (inc n))))))

(defn result [input n]
  (inc (count (take-while (complement (partial n-zeroes? n))
                          (hashes input)))))

(defn day4 []
  (let [input "yzbqklnj"]
    (pmap (partial result input) [5 6])))
