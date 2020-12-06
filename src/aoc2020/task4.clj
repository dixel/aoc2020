(ns aoc2020.task4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def obligatory-fields #{"byr"
                         "iyr"
                         "eyr"
                         "hgt"
                         "hcl"
                         "ecl"
                         "pid"})

(defn validate-int [i low high]
  (let [n (try
            (Integer/parseInt i)
            (catch Exception e
              nil))]
    (and (>= n low) (<= n high))))

(defn height-checker [hgt unit low high]
  (let [h (-> hgt
              (str/split (re-pattern unit))
              (first))]
    (validate-int h low high)))

(defn check-height [hgt]
  (cond
    (re-matches #"[0-9]*cm" hgt) (height-checker hgt "cm" 150 193)
    (re-matches #"[0-9]*in" hgt) (height-checker hgt "in" 59 76)))

(defn is-valid-field? [[name value]]
  (when
      (case name
        "byr" (validate-int value 1920 2002)
        "iyr" (validate-int value 2010 2020)
        "eyr" (validate-int value 2020 2030)
        "hgt" (check-height value)
        "hcl" (re-matches #"#[0-9a-fA-F]{6}" value)
        "ecl" (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} value)
        "pid" (re-matches #"[0-9]{9}" value)
        false)
    name))

(defn is-passport-valid? [passport]
  (-> passport
      (str/split #"\n| ")
      ((fn [e] (->> (map #(-> (str/split % #":") is-valid-field?) e)
                    (filter (complement nil?))
                    (into #{})
                    (#(set/subset? obligatory-fields %)))))))
                 
(def input (-> (slurp "resources/4/input")
               (str/split #"\n\n")))

(println (->> input
              (map is-passport-valid?)
              (filter true?)
              (count)))
