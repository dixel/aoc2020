(ns aoc2020.task11
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/11/input")
      (str/split-lines)
      (map-indexed (fn [i row]
                     [i (into {}
                              (map-indexed (fn [i col]
                                             [i col])
                                           row))]))

      (into {})))

(def debug
  (->>
   "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
   (str/split-lines)
   (map-indexed (fn [i row]
                  [i (into {}
                           (map-indexed (fn [i col]
                                          [i col])
                                        row))]))
   (into {})))

(first debug)


(defn get-neighbour-idxs [[row col :as idx] depth]
  [[row (+ depth col)]
   [row (- col depth)]
   [(+ depth row) col]
   [(+ depth row) (+ depth col)]
   [(+ depth row) (- col depth)]
   [(- row depth) col]
   [(- row depth) (+ depth col)]
   [(- row depth) (- col depth)]])

(defn get-visible-neighbours [[row col :as idx] seats]
  (loop [step 1
         neighbour-idxs (get-neighbour-idxs idx step)
         result {}]
    (if (= (count result) 8)
      (vals result)
      (recur (inc step) (get-neighbour-idxs idx (inc step))
             (reduce (fn [acc [idx n]]
                       (cond (contains? acc idx) acc
                             (= (get-in seats n) \.) acc
                             :else (assoc acc idx (get-in seats n))))
                     result
                     (map-indexed vector neighbour-idxs))))))

(defn swap-state [element idx occupied-cnt neighbour-fn]
  (let [neighbours (neighbour-fn idx)]
    (cond
      (and (= element \L)
           (not (contains? (into #{} neighbours) \#)))
      \#
      (and (= element \#)
           (>= (-> neighbours
                  frequencies
                  (get \# 0))
               occupied-cnt))
      \L
      :else element)))

(defn calculate-step [seats-state neighbours-fn occupied-cnt]
  (loop [result {}
         i (- (count seats-state) 1)
         j (- (count (second (first seats-state))) 1)]
    (if (= -1 i)
      result
      (recur
       (assoc-in result [i j] (swap-state (get-in seats-state [i j]) [i j]
                                          occupied-cnt
                                          #(neighbours-fn % seats-state)))
       (if (= 0 j) (- i 1) i)
       (if (= 0 j) (- (count (second (first seats-state))) 1) (- j 1))))))

(defn output-seats [seats]
  (doseq [i (range (count seats))]
    (->> (map #(get-in seats [i %] "@") (range (count (second (first seats)))))
         (str/join "")
         (println))))

(defn count-occupied-seats [seats neighbours-fn occupied-cnt]
  (loop [previous-seats-state seats
         iter 0
         next-seats-state     (calculate-step previous-seats-state neighbours-fn occupied-cnt)]
    (if (= next-seats-state previous-seats-state)
      (->> next-seats-state
           vals
           (map #(-> %
                     vals
                     flatten))
           flatten
           frequencies
           (#(get % \#)))
      (recur next-seats-state (inc iter) (calculate-step next-seats-state neighbours-fn occupied-cnt)))))

(count-occupied-seats debug
                      (fn [idx seats-state]
                        (map #(get-in seats-state %) (get-neighbour-idxs idx 1))) 4)

(count-occupied-seats debug get-visible-neighbours 5)
