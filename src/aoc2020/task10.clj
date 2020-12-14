(ns aoc2020.task10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def input (->> (slurp "resources/10/input")
               (str/split-lines)
               (map #(Long/parseUnsignedLong %))))

(def enriched-input
  (let [internal (+ 3 (apply max input))]
    (-> input
        (conj 0)
        (concat [internal])
        (sort)
        (reverse))))

(defn get-adapter-chain [input]
  (->> (partition 2 (interleave input (rest input)))
       (map #(apply - %))))

; Task 1

(->> (get-adapter-chain enriched-input)
     frequencies
     vals
     (apply *))

(defn sum-up-to-n [options n res]
  (if (= n 0)
    [res]
    (loop [opts options
           result []]
      (if (empty? opts)
        result
        (let [o (first opts)]
          (if (>= n o)
            (recur (rest opts) (concat result (sum-up-to-n options (- n o) (conj res o))))
            (recur (rest opts) result)))))))

(defn sum-up-to-n [])

(defn consecutive-ones [coll]
  (loop [res []
         acc 0
         consecutive? false
         in coll]
    (if (empty? in)
      (if (> acc 0)
        (conj res acc)
        res)
      (let [[element & next] in]
        (cond
          (and consecutive? (= element 1)) (recur res (inc acc) true next)
          consecutive? (recur (conj res acc) 0 false next)
          (= element 1) (recur res 1 true next)
          :else (recur res 0 false next))))))

(defn consecutive-ones [coll]
  (-> coll
      ))

(println
 (->> (get-adapter-chain enriched-input)
      consecutive-ones
      (map #(sum-up-to-n [1 2 3] % []))
      (map count)
      (reduce *)))

(sum-up-to-n [1 2 3] 4 [])

(println
 (->> [1 3 1 1 1 1 3 1 1 1 3]
      consecutive-ones
      (map #(sum-up-to-n [1 2 3] % []))
      (map count)
      (reduce *)))

;; better solution peeped in twitter
(defn count-arrangements [adapters adapter]
  (reduce (fn [cnts n]
            (assoc cnts n (+ (cnts (+ n 1) 0)
                             (cnts (+ n 2) 0)
                             (cnts (+ n 3) 0))))
          {(first adapters) 1}
          (rest adapters)))

(sort
 (count-arrangements enriched-input 0))
