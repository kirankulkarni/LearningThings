(ns freq.core
  (:import (java.io BufferedReader FileReader))
  (:require [clojure.string :as string]))

(defn read-file-lazy
  "Opens a file and creates a lazy-sequence to read each line"
  [file-name]
  (with-open [reader  (BufferedReader. (FileReader. file-name))]
    (line-seq reader)))

(defn read-file
  "Reads a file using slurp"
  [file-name]
  (slurp file-name))

(defn read-words
  "returns the words in a file"
  [file-name]
  (re-seq #"\w+" (read-file file-name)))

(defn update-frequencies
  "Update frequency of the word"
  [frequency-map word]
  (let [lword (string/lower-case word)]
    (assoc frequency-map lword (inc (frequency-map lword 0)))))

(defn calculate-word-frequencies
  "Given a sequence of words spits out frequency map of each word"
  [words]
  (reduce update-frequencies {} words))

(defn top-words
  "Get top 10 frequent words from file"
  [file-name]
  (take 10 (reverse (sort-by val (calculate-word-frequencies (read-words file-name))))))