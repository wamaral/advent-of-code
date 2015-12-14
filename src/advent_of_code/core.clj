(ns advent-of-code.core
  (:gen-class)
  (:require [advent-of-code.day1]
            [advent-of-code.day2]
            [advent-of-code.day3]
            [advent-of-code.day4]
            [advent-of-code.day5]
            #_[advent-of-code.day6]
            #_[advent-of-code.day7]
            #_[advent-of-code.day8]
            #_[advent-of-code.day9]
            #_[advent-of-code.day10]
            #_[advent-of-code.day11]
            #_[advent-of-code.day12]
            #_[advent-of-code.day13]
            #_[advent-of-code.day14]
            #_[advent-of-code.day15]
            #_[advent-of-code.day16]
            #_[advent-of-code.day17]
            #_[advent-of-code.day18]
            #_[advent-of-code.day19]
            #_[advent-of-code.day20]
            #_[advent-of-code.day21]
            #_[advent-of-code.day22]
            #_[advent-of-code.day23]
            #_[advent-of-code.day24]
            #_[advent-of-code.day25]))

(defn -main [input & args]
  (println "Advent of code!")
  (let [sym (symbol (str "advent-of-code.day" input "/day" input))]
    (println (str "Running " sym))
    ((resolve sym))))
