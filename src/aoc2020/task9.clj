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
         in (drop 25 input)
         index 26]
    (cond
      (empty? in) nil
      (not (xmas-check (first in) previous)) [(first in) index]
      :else (recur (conj (vec (drop 1 previous)) (first in)) (rest in) (inc index)))))

(defn sublists [l]
  (->> (for [i (range 0 (count l))
             j (range 0 (count l))]
        (if (>= j i)
          (subvec l i j)
          []))
       (filter (complement empty?))))

(defn find-vulnerability [num input]
  (loop [perms (sublists (vec input))]
    (let [sublist (first perms)]
      (if (= (reduce + sublist) num)
        (+ (apply min sublist) (apply max sublist))
        (recur (rest perms))))))

(println
 (let [[num index] (find-corrupted-number input)]
   (find-vulnerability num (take index input))))
