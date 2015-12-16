(ns advent-of-code.day6)

(def on1 (constantly 1))
(def off1 (constantly 0))
(def toggle1 #(if (= 0 %) 1 0))

(def on2 inc)
(def off2 #(max 0 (dec %)))
(def toggle2 (comp inc inc))

(defn affected? [x y line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn compute-state [x y lines]
  (let [affected (filter (partial affected? x y) lines)]
    (reduce #((resolve (:action %2)) %1) 0 affected)))

(defn make-grid [width]
  (for [x (range 0 width)
        y (range 0 width)]
    [x y]))

(defn compute-grid [grid lines]
  (pmap #(compute-state (first %) (last %) lines)
        grid))

(defn parse-line [mode line]
  (let [[_ action] (re-find #"(\w+) \d" line)
        [x1 y1 x2 y2] (map #(Integer/parseInt %)
                           (rest (re-find #"(\d+),(\d+) through (\d+),(\d+)" line)))]
    {:action (symbol (str action mode))
     :x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn day6 []
  (with-open [rdr (clojure.java.io/reader "resources/day6.input")]
    (let [lines (line-seq rdr)
          day1-lines (map (partial parse-line 1) lines)
          day2-lines (map (partial parse-line 2) lines)
          width 1000
          grid (make-grid width)]
      (println (str "6.1: " (reduce + (compute-grid grid day1-lines))))
      (println (str "6.2: " (reduce + (compute-grid grid day2-lines)))))))
