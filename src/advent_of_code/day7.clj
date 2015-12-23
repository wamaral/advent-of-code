(ns advent-of-code.day7)

;;; SICP, chapter 3.3.4

(defn make-time-segment [time queue]
  '(time queue))
(def segment-time first)
(def segment-queue second)

(defn make-agenda []
  (list 0))

(defn current-time [agenda]
  (second agenda))

(defn set-current-time! [agenda time]
  ())

(def agenda (make-agenda))
(def inverter-delay 2)
(def and-delay 3)
(def or-delay 5)

(defn get-signal
  "returns the current value of the signal on the wire."
  [wire]
  (wire 'get-signal))

(defn set-signal!
  "changes the value of the signal on the wire to the new value."
  [wire val]
  ((wire 'set-signal!) val))

(defn add-action!
  "asserts that the designated procedure should be run whenever the signal on the wire changes value. Such procedures are the vehicles by which changes in the signal value on the wire are communicated to other wires."
  [wire f]
  ((wire 'add-action!) f))

(defn probe [name wire]
  (add-action! wire
               (fn [] (println (str name " " (current-time agenda) " New-value = " (get-signal wire))))))

(defn after-delay [delay action]
  (add-to-agenda! (+ delay (current-time agenda))
                  action
                  agenda))

(defn propagate []
  (if (empty-agenda? agenda)
    'done
    (let [first-item (first-agenda-item agenda)]
      (first-item)
      (remove-first-agenda-item! agenda)
      (propagate))))

(defn logical-not [s]
  (if (= s 0) 1 0))

(defn logical-and [s1 s2])

(defn logical-or [s1 s2])

(defn inverter [input output]
  (defn invert-input []
    (let [new-value (logical-not (get-signal input))]
      (after-delay inverter-delay
                   (fn [] (set-signal! output new-value)))))
  (add-action! input invert-input))

(defn and-gate [a1 a2 output]
  (defn and-action-procedure []
    (let [new-value (logical-and (get-signal a1) (get-signal a2))]
      (after-delay and-delay
                   (fn [] (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(defn or-gate [a1 a2 output]
  (defn or-action-procedure []
    (let [new-value (logical-or (get-signal a1 (get-signal a2)))]
      (after-delay or-delay
                   (fn [] (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

(defn make-wire []
  (let [signal-value 0
        action-procedures '()]
    (defn set-my-signal! [new-value]
      (if (not (= signal-value new-value))
        (doall (set! signal-value new-value)
               (map resolve action-procedures))))
    (defn accept-action-procedure! [f]
      (set! action-procedures (cons f action-procedures))
      (f))
    (defn dispatch [m]
      (cond ((= m 'get-signal) signal-value)
            ((= m 'set-signal!) set-my-signal!)
            ((= m 'add-action!) accept-action-procedure!)))
    dispatch))

(defn parse-line [line]
  (let [[_ input output] (re-find "^(.*) -> (.*)$" line)]))

(defn schematize [lines]
  )

(def node-value
  (memoize
   (fn [schema node]
     )))

(defn day7 []
  (with-open [rdr (clojure.java.io/reader "resources/day7.input")]
    (let [lines (map parse-line (line-seq rdr))
          schema (schematize lines)])))

(day7)
