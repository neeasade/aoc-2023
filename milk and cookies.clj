#!/usr/bin/env bb

(require '[clojure.java.shell :as shell]
         '[clojure.string :as string]
         '[babashka.fs :as fs])

(defn get-input [day & split?]
  (let [split? (if (nil? split?) true false)
        cache-file (fs/file (fs/xdg-cache-home) (format "aoc/%s.txt" day))]
    (fs/create-dirs (fs/parent cache-file))
    (when-not (fs/exists? cache-file)
      (spit cache-file
            (:out (shell/sh
                   "curl" (format "https://adventofcode.com/2023/day/%s/input" day)
                   "-X" "GET"
                   "-H" (format "Cookie: session=%s" (System/getenv "AOC_SESSION")))))) ; https://github.com/wimglenn/advent-of-code-wim/issues/1
    (if split?
      (string/split-lines (slurp cache-file))
      (slurp cache-file))))

;; day 1 part 2
;; not solved - works on example input but not real input
(let [patterns ["[0-9]" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]]
  (->> (get-input "1")
       (map (fn [line]
              (->> line
                   (re-seq (re-pattern (string/join "|" patterns)))
                   ((juxt first last))
                   (map (fn [num]
                          (let [index (.indexOf patterns num)]
                            (if (= -1 index) num index))))
                   (string/join)
                   (read-string))))
       (apply +)))

;; day 1 part 1
(->> (get-input 1)

     (map (fn [line]
            (->> line
                 (re-seq #"[0-9]")
                 ((juxt first last))
                 (string/join)
                 (read-string))))
     (apply +))
