(ns advent-of-code.day1)

(defn step [acc x]
  (let [op (if (= \( x) inc dec)]
    (eval (op acc))))

(defn day1 []
  (let [input (slurp "resources/day1.input")]
    (println (str "Final floor: " (reduce step 0 input)))
    (println (str "Steps to basement: " (count (take-while #(>= % 0)
                                                           (reductions step 0 input)))))))
