(ns advent-of-code.day5)

(defn nice? [str]
  (and (> (count (filter #(#{\a \e \i \o \u} %) str)) 2)
       (re-find #"(.)\1" str)
       (not (re-find #"(?:ab|cd|pq|xy)" str))))

(with-open [rdr (clojure.java.io/reader "resources/day5.input")]
  (println (str "5.1: " (count (filter true? (map nice? (line-seq rdr)))))))
