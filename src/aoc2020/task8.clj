(ns aoc2020.task8
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/8/input")
      (str/split-lines)
      (map #(-> %
                (str/split #" ")))
      (map #(identity [(first %) (Integer/parseInt (second %))]))
      (map-indexed vector)
      (into {})))

(defn find-acc-before-loop [input]
  (loop [acc     0
         pc      0
         visited #{}]
    (let [[ir dr] (input pc)]
      (cond
        (visited pc) [true acc]
        (> pc (count input)) [false acc]
        :else
        (recur (if (= "acc" ir)
                 (+ acc dr)
                 acc)
               (if (= "jmp" ir)
                 (+ pc dr)
                 (inc pc))
               (conj visited pc))))))

(defn flip-jmp [[cmd data]]
  (case cmd
    "jmp" ["nop" data]
    "nop" ["jmp" data]
    [cmd data]))

(defn find-fixed-acc [input]
  (loop [pc      0
         input   input]
    (let [[ir dr]      (input pc)
          [cycle? acc] (find-acc-before-loop (update input pc flip-jmp))]
      (if cycle?
        (recur (inc pc) input)
        acc))))




(find-acc-before-loop input)
(find-fixed-acc input)
