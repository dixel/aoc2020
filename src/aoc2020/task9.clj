(ns aoc2020.task9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def input
  (->> (slurp "resources/9/input")
       (str/split-lines)
       (map #(Long/parseUnsignedLong %))))

(defn xmas-check [num previous]
  (if (some #(= (reduce + %) num) (comb/combinations previous 2))
    true
    false))

(defn find-corrupted-number [input]
  (loop [previous (into [] (take 25 input))
         in (drop 25 input)]
    (cond
      (empty? in) nil
      (not (xmas-check (first in) previous)) (first in)
      :else (recur (conj (vec (drop 1 previous)) (first in)) (rest in)))))

(defn sublists [l]
  (->> (for [i (range 0 (count l))
             j (range 0 (count l))]
        (if (>= j i)
          (subvec l i j)
          []))
       (filter (complement empty?))))


(sublists [1 2 3 4])

(defn find-vulnerability [num input]
  (for [sublist (sublists input)]
    (if (= (reduce + sublist) num)
      (println (min sublist) ":" (max sublist)))))

(println (find-corrupted-number input))
