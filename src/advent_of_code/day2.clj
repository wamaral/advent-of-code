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

(defn ribbon [acc box]
  (let [[l w h] (map read-string (re-seq #"\d+" box))
        bow (* l w h)
        mins (take 2 (sort [l w h]))
        wrap (reduce + (map #(* 2 %) mins))]
    (+ acc bow wrap)))

(with-open [rdr (clojure.java.io/reader "resources/day2.input")]
  (println (str "Total paper: " (reduce sum-area 0 (line-seq rdr)))))

(with-open [rdr (clojure.java.io/reader "resources/day2.input")]
  (println (str "Total ribbon: " (reduce ribbon 0 (line-seq rdr)))))
