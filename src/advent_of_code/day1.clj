(ns advent-of-code.day1)

(defn count-char [char input]
  (get (frequencies input) char))

(let [input (slurp "resources/day1.input")]
  (println (- (count-char \( input)
              (count-char \) input))))
