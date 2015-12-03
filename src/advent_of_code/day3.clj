(ns advent-of-code.day3)

(defn step [acc cmd]
  (let [ops (cond (= \^ cmd) [identity inc]
                  (= \> cmd) [inc identity]
                  (= \v cmd) [identity dec]
                  (= \< cmd) [dec identity])
        pairs (interleave ops acc)
        x (take 2 pairs)
        y (drop 2 pairs)]
    [(eval x) (eval y)]))

(let [input (slurp "resources/day3.input")]
  (println (str "Houses with at least one present: "
                (count (distinct (reductions step [0 0] input))))))
