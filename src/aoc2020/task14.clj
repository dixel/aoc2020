(ns aoc2020.task14
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/14/input")
       (str/split-lines)))

(defn parse-mask [line]
  {:command :mask
   :value (into [] (second (str/split line #"mask = ")))})

(defn parse-mem [line]
  {:command :mem
   :address (Long/parseUnsignedLong (second (re-find #"\[(.*)\]" line)))
   :value (reverse (take 36 (concat (reverse (Long/toBinaryString (Long/parseUnsignedLong (second (re-find #"= (.*)" line))))) (repeat \0))))})

(defn parse-line [line]
  (if (str/starts-with? line "mask")
    (parse-mask line)
    (parse-mem line)))

(defn parse-program [program]
  (map parse-line program))

(defn truth-table [s-mask s-num]
  (cond
    (= s-mask \X) s-num
    :else s-mask))

(defn apply-mask [mask num]
  (Long/parseUnsignedLong (apply str (map truth-table mask num)) 2))

(defn part-1 [program]
  (loop [commands (parse-program program)
         mem {}
         mask (take 36 (repeat \X))]
    (if (empty? commands)
      (reduce + (vals mem))
      (let [{:keys [command address value]} (first commands)]
        (case command
          :mask (recur (rest commands) mem value)
          :mem (recur (rest commands) (assoc mem address (apply-mask mask value)) mask))))))
