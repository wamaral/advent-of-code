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

(defn with-robot [acc cmds]
  [(step (first acc) (first cmds))
   (step (last acc) (last cmds))])

(let [input (slurp "resources/day3.input")
      split-input (partition 2 input)
      houses-santa (reductions step [0 0] input)
      houses-robot (reductions with-robot [[0 0] [0 0]] split-input)]
  (println (str "Houses with at least one present: " (count (distinct houses-santa))))
  (println (str "Houses with robot: " (count (distinct (apply concat houses-robot))))))
