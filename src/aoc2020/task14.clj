(ns aoc2020.task14
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/14/input")
       (str/split-lines)))

(defn parse-mask [line]
  {:command :mask
   :binary-value (into [] (second (str/split line #"mask = ")))})

(defn pad-bit-list [bit-list n]
  (-> bit-list
      (reverse)
      (concat (repeat \0))
      ((partial take n))
      (reverse)))

(defn parse-mem [line]
  (let [address (Long/parseUnsignedLong (second (re-find #"\[(.*)\]" line)))
        value (Long/parseUnsignedLong (second (re-find #"= (.*)" line)))]
    {:command :mem
     :address address
     :binary-address (pad-bit-list (Long/toBinaryString address) 36)
     :value value
     :binary-value (pad-bit-list (Long/toBinaryString value) 36)}))

(defn parse-line [line]
  (if (str/starts-with? line "mask")
    (parse-mask line)
    (parse-mem line)))

(defn parse-program [program]
  (map parse-line program))

(defn truth-table [s-mask s-num]
  (case s-mask
    \X s-num
    s-mask))

(defn truth-table-addr [s-mask s-addr]
  (case s-mask
    \X \X
    \1 \1
    \0 s-addr))

(defn get-address-set [addr]
  (let [floating-addr (into [] addr)
        bits (-> floating-addr
                  frequencies
                  (get \X))
        combs (bit-shift-left 1 bits)
        bstrings (->> (range combs)
                      (map #(pad-bit-list (Long/toBinaryString %) bits))
                      (map #(into [] %)))
        x-indexes (->> floating-addr
                       (map-indexed vector)
                       (filter #(= (second %) \X))
                       (map first))]
    (->> bstrings
         (map #(apply assoc floating-addr (interleave x-indexes %)))
         (map #(apply str %))
         (map #(Long/parseUnsignedLong % 2)))))

(defn apply-mask [mask num truth-table]
  (map truth-table mask num))

(defn bit-list-to-num [bit-list]
  (Long/parseUnsignedLong (apply str bit-list) 2))

(defn mem-sum [program part]
  (loop [commands (parse-program program)
         mem {}
         mask (take 36 (repeat \X))]
    (if (empty? commands)
      (reduce + (vals mem))
      (let [{:keys [command binary-address address binary-value value]} (first commands)]
        (case command
          :mask (recur (rest commands) mem binary-value)
          :mem (recur (rest commands) (if (= part 1)
                                        (assoc mem address (bit-list-to-num (apply-mask mask binary-value truth-table)))
                                        (apply assoc mem (-> (apply-mask mask binary-address truth-table-addr)
                                                           get-address-set
                                                           (interleave (repeat value)))))
                       mask))))))

(println (mem-sum input 1))
(println (mem-sum input 2))
