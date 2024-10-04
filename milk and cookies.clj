#!/usr/bin/env bb

(require '[clojure.java.shell :as shell]
         '[clojure.string :as string]
         '[clojure.math :as math]
         '[clojure.set :as set]
         '[babashka.fs :as fs])


(def AOC_SESSION (string/trim (:out (shell/sh "bash" "-i" "-c" "echo $AOC_SESSION"))))

(defn get-input [day & split?]
  (let [split? (if (nil? split?) true false)
        ;; quicklink: ~/.cache/aoc
        cache-file (fs/file (fs/xdg-cache-home) (format "aoc/%s.txt" day))]
    (fs/create-dirs (fs/parent cache-file))
    (when-not (fs/exists? cache-file)
      (spit cache-file
            (:out (shell/sh
                   "curl" (format "https://adventofcode.com/2023/day/%s/input" day)
                   "-X" "GET"
                   "-H" (format "Cookie: session=%s" AOC_SESSION))))) ; https://github.com/wimglenn/advent-of-code-wim/issues/1
    (if split?
      (string/split-lines (slurp cache-file))
      (slurp cache-file))))


;; day 4 part 1
(->> (get-input "4")
     (map (fn [line]
            (let [[_ p1 p2] (string/split line #"[:|]" )
                  s1 (->> (string/split p1 #" ") (keep parse-long) (set))
                  s2 (->> (string/split p2 #" ") (keep parse-long) (set))]
              (if (empty? (set/intersection s1 s2)) 0
                  (math/pow 2 (dec (count (set/intersection s1 s2)))))
              p1)))
     ;; (apply +)
     first)

;; day 3

;; https://stackoverflow.com/questions/3262195/compact-clojure-code-for-regular-expression-matches-and-their-position-in-string
(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(comment
  (re-pos #"\d+" "...34...5") ;; {3 "34", 8 "5"}
  (re-pos #"\d+" "......")) ;; {}

;; part 1
(let [input (get-input "3")
      width (count (first input))
      pad (apply str (repeat width \.))
      input `[~pad ~@input ~pad]
      ;; ... lmfao
      input (map (fn [line] (str "." line ".")) input)]

  (->> (partition 3 1 input)
       (map (fn [[prev cur next]]
              (->> (re-pos #"\d+" cur)
                   (keep (fn [[pos match]]
                           ;; (prn cur)
                           ;; (prn pos)
                           ;; (prn match)
                           (let [search-string (str (subs prev (dec pos) (+ (inc pos) (count match)))
                                                    (nth cur (dec pos))
                                                    (nth cur (+ pos (count match)))
                                                    (subs next (dec pos) (+ (inc pos) (count match))))]
                             (when-not (every? (partial = \.) search-string)
                               match
                               )))))))
       (flatten)
       (map #(Integer/parseInt %))
       (apply +)))

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
      (if (or          ; only 12 red cubes, 13 green cubes, and 14 blue cubes
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
