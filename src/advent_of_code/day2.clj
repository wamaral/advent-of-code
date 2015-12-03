(ns advent-of-code.day2)

(defn sum-area [acc box]
  (let [[l w h] (map read-string (re-seq #"\d+" box))
        x (* l w)
        y (* w h)
        z (* h l)]
    (+ acc
       (* 2 x)
       (* 2 y)
       (* 2 z)
       (min x y z))))

(with-open [rdr (clojure.java.io/reader "resources/day2.input")]
  (reduce sum-area 0 (line-seq rdr)))
