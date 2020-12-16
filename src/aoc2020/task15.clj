(ns aoc2020.task15)

(def input (->> [0,1,4,13,15,12,16]
                (map-indexed (fn [k v] [v (inc k)]))
                (into {})))

(defn count-step [board current step]
  (let [previous-index (get board current)]
    (case previous-index
      nil 0
      (- step previous-index))))

(loop [board input
       current 16
       step (count input)]
  (if (= 2020 step)
    current
    (recur (assoc board current step) (count-step board current step) (inc step))))
