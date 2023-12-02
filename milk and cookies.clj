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

;; day 2 part 2
(defn line-to-num [line]
  (->> line
       (re-seq #"(\d+) (red|green|blue)")
       (map (fn [[_ amount color]]
              {(keyword color) [(Integer/parseInt amount)]}))
       (apply merge-with concat)
       (vals)
       (map #(first (sort > %)))
       (apply *)))

(->> (get-input "2")
     (map line-to-num)
     (apply +))

;; day 2 part 1
(defn line-to-num [line]
  ;; return the game id (if matching), or 0
  (let [id (Integer/parseInt (re-find #"\d+" line))
        seen (->> line
                  (re-seq #"(\d+) (red|green|blue)")
                  (map (fn [[_ amount color]]
                         {(keyword color) [(Integer/parseInt amount)]}))
                  (apply merge-with concat))]

    (let [{:keys [red green blue]} seen]
      (if (or             ; only 12 red cubes, 13 green cubes, and 14 blue cubes
           (some #(> % 12) red)
           (some #(> % 13) green)
           (some #(> % 14) blue))
        0 id
        ))))

(->> (get-input "2")
     (map line-to-num)
     (apply +))

;; day 1 part 2
;; notes:
;; positive lookahead: https://www.regular-expressions.info/lookaround.html, (?=<pattern>)
(comment
  ;; so eg oneight = "one" "eight" instead of just "one"
  (re-seq #"one|eight" "oneight") ;; ("one")
  (re-seq #"(?=(one|eight))" "oneight") ;; (["" "one"] ["" "eight"])
  (re-seq #"(?=one|eight)" "oneight")) ;; ("" "")

(let [patterns ["[0-9]" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]]
  (->> (get-input "1")
       (map (fn [line]
              (->> line
                   (re-seq (re-pattern (format "(?=(%s))" (string/join "|" patterns))))
                   (map last)
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
