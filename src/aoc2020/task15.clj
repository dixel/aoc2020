(ns aoc2020.task15)

(def source [0,1,4,13,15,12,16])
(def input (->> source
                (map-indexed (fn [k v] [v (inc k)]))
                (into {})))

(defn count-step [board current step]
  (let [previous-index (get board current)]
    (case previous-index
      nil 0
      (- step previous-index))))

(defn get-nth-round [game previous previous-index n]
  (loop [board game
         current previous
         step previous-index]
    (if (= n step)
      current
      (recur (assoc board current step) (count-step board current step) (inc step)))))

(defn print-results []
  (println "task 1: " (get-nth-round input (last source) (count source) 2020))
  (println "task 2: " (get-nth-round input (last source) (count source) 30000000)))
