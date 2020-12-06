(ns aoc2020.core
  (:require [clojure.string :as str]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn is-tree? [line i]
  (= (nth (flatten (repeat (into [] line))) i) \#))

(defn downhill [text right down]
  (loop [slope text
         iright right
         trees 0]
    (if (<= (count slope) down)
      trees
      (recur (drop down slope)
             (+ right iright)
             (let [l (nth slope down)]
               (if (is-tree? l iright)
                 (do
                   (println "line: " l ": " iright ": true")
                   (inc trees))
                 (do
                   (println "line: " l ": " iright ": false")
                   trees)))))))

(def input
  (-> (slurp "resources/3/input")
      (str/split #"\n")))
     
(reduce * (map #(downhill input (first %) (second %)) [[1 1] [3 1] [5 1] [7 1] [1 2]]))
