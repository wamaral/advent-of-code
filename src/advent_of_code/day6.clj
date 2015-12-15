(ns advent-of-code.day6)

(def on (constantly true))
(def off (constantly false))
(def toggle not)

(defn affected? [x y line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn compute-state [x y lines]
  (let [affected (filter (partial affected? x y) lines)]
    (reduce #((resolve (:action %2)) %1) false affected)))

(defn make-grid [width]
  (for [x (range 0 width)
        y (range 0 width)]
    [x y]))

(defn compute-grid [grid lines]
  (pmap #(compute-state (first %) (last %) lines)
        grid))

(defn parse-line [line]
  (let [[_ action] (re-find #"(\w+) \d" line)
        [x1 y1 x2 y2] (map #(Integer/parseInt %)
                           (rest (re-find #"(\d+),(\d+) through (\d+),(\d+)" line)))]
    {:action (symbol action)
     :x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn day6 []
  (with-open [rdr (clojure.java.io/reader "resources/day6.input")]
    (let [lines (map parse-line (line-seq rdr))
          width 1000
          grid (make-grid width)]
      (println (str "6.1: " (count (filter true? (compute-grid grid lines))))))))
