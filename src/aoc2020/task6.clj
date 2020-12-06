(ns aoc2020.task6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> (slurp "resources/6/input")
      (str/split #"\n\n")))

(defn check-group [group]
  (let [answers (str/split group #"\n")]
    (->> answers
         (map #(into #{} %))
         (reduce set/intersection) ; (reduce set/union) for the first task
         (count))))

(println (->> input
              (map check-group)
              (reduce +)))
