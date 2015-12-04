(ns advent-of-code.day4
  (:require digest))

(defn five-zeroes? [md5]
  (= "00000"
     (subs md5 0 5)))

(defn get-hashes
  ([input] (get-hashes input 1))
  ([input n] (cons (digest/md5 (str input n))
                   (lazy-seq (get-hashes input (inc n))))))

(let [input "yzbqklnj"]
  (inc (count (take-while (complement five-zeroes?) (get-hashes input)))))
