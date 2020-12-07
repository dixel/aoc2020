(ns aoc2020.task7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-contents [contents]
  (-> contents
      (str/split #", ")
      ((partial map (juxt #(str/replace % #"\d+ " "") #(->> %
                                                            (re-find #"\d+")
                                                            (Integer/parseUnsignedInt)))))))

(defn parse-record [record]
  (-> record
      (str/replace "bags" "bag")
      (str/replace "." "")
      (str/replace "no other bag" "")
      (str/replace " bag" "")
      (str/split #" contain ")
      (#(identity [(first %) (into {} (when (second %) (parse-contents (second %))))]))))


(defn bag-occurences [color existing input]
  (let [new-containers (->> input
                            (filter #(contains? (second %) color))
                            (map first)
                            (into #{}))]
    (if (empty? new-containers)
      existing
      (do
        (->> new-containers
             (map #(bag-occurences % (set/union existing new-containers) input))
             (map #(into [] %))
             (flatten)
             (into #{}))))))

(defn bags-inside [color input]
  (if (empty? (input color))
    0
    (reduce #(+ %1 (second %2) (* (second %2) (bags-inside (first %2) input))) 0 (input color))))

(def input (->> (slurp "resources/7/input")
               (str/split-lines)
               (map parse-record)
               (into {})))

(count (bag-occurences "shiny gold" #{} input))
(bags-inside "shiny gold" input)
