(ns advent-of-code.day5)

(defn nice1? [str]
  (and (> (count (filter #(#{\a \e \i \o \u} %) str)) 2)
       (re-find #"(.)\1" str)
       (not (re-find #"(?:ab|cd|pq|xy)" str))))

(defn nice2? [str]
  (boolean (and (re-find #"(..).*\1" str)
                (re-find #"(.).\1" str))))

(defn day5 []
  (with-open [rdr (clojure.java.io/reader "resources/day5.input")]
    (let [lines (line-seq rdr)]
      (println (str "5.1: " (count (filter true? (map nice1? lines)))))
      (println (str "5.2: " (count (filter true? (map nice2? lines))))))))
