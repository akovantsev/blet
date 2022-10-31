(ns com.akovantsev.blet.maze
  (:require
   [clojure.edn :as edn]
   [com.akovantsev.blet.core :refer [blet blet!]]
   [clojure.string :as str]
   [clojure.set :as set]))


(def INPUT {::gridsize [8 8]
            ::entrance "A1"
            ::walls    ["C1", "G1", "A2", "C2", "E2", "G2", "C3", "E3", "B4", "C4", "E4", "F4", "G4", #_"B5", "E5", "B6", "D6", "E6", "G6", "H6", "B7", "D7", "G7", "B8"]})

(def parse-x (into {} (map vector (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (rest (range)))))
(def parse-y edn/read-string)

(def unparse-x (set/map-invert parse-x))

(defn id->xy [id]
  (when-let [[_ x y] (re-matches #"([a-zA-Z]+)(\d+)" id)]
    [(parse-x x) (parse-y y)]))


(id->xy "A1")

(->> INPUT ::walls (map id->xy))


(defn solve [input]
  (blet [[maxx maxy] (->> input ::gridsize)
         entrance (->> input ::entrance id->xy)
         wall?    (->> input ::walls (map id->xy) set)
         exit     (->>
                    (map vector (range maxx 0 -1) (repeat maxy))
                    (remove #{entrance})
                    (remove wall?)
                    (first)) ;;rightmost
         _ (assert exit)
         cell      (fn [path? xy]
                     (cond
                       (wall? xy)      "#"
                       (= entrance xy) "@"
                       (= exit xy)     "*"
                       (path? xy)      "."
                       :else           " "))
         xs       (range 1 (inc maxx))
         ys       (range 1 (inc maxy))
         render   (fn [path]
                    (->> ys
                      (map (fn [y]
                             (->> xs
                               (map (fn [x] (cell (set path) [x y])))
                               (str/join)
                               (str y))))
                      (str/join "\n")
                      (str " " (str/join (map unparse-x xs)) "\n")
                      (println)))
         opts     (memoize
                    (fn [[x y]]
                      (let [x- (dec x), x+ (inc x)
                            y- (dec y), y+ (inc y)]
                        (->>
                          [(when (<= 1 y-)    [x  y-])  ;;up
                           (when (<= 1 x-)    [x- y])   ;;left
                           (when (<= x+ maxx) [x+ y])   ;;right
                           (when (<= y+ maxy) [x  y+])];;down
                          (remove nil?)
                          (remove wall?)))))]
    ;(println "maze:")
    ;(render #{})
    (loop [todo     [[#{} 1 [entrance]]]
           shortest nil
           longest  nil]
      (blet [[seen pathlen path] (peek todo)
             xy        (peek path)
             todo-     (pop todo)
             seen+     (conj seen xy)
             xys       (->> xy opts (remove seen))
             pathlen+  (inc pathlen)
             todos     (map (fn [xy] [seen+ pathlen+ (conj path xy)]) xys)
             todo+     (into todo- todos)
             shortlen  (if shortest (count shortest) Integer/MAX_VALUE)
             longlen   (if longest (count longest) 0)
             shortest* (if (< pathlen shortlen) path shortest)
             longest*  (if (< longlen pathlen) path longest)]
        (cond
          (empty? todo) (do
                          ;(println "shortest:")
                          ;(render shortest)
                          ;(println "longest:")
                          ;(render longest)
                          [shortest longest])
          ;(< shortlen pathlen) (recur todo- shortest longest)
          (= exit xy)   (recur todo- shortest* longest*)
          (empty? xys)  (recur todo- shortest longest)
          :else         (recur todo+ shortest longest))))))


(defn solve2 [input]
  (let [[maxx maxy] (->> input ::gridsize)
        entrance (->> input ::entrance id->xy)
        wall?    (->> input ::walls (map id->xy) set)
        exit     (->>
                   (map vector (range maxx 0 -1) (repeat maxy))
                   (remove #{entrance})
                   (remove wall?)
                   (first)) ;;rightmost
        _ (assert exit)
        cell      (fn [path? xy]
                    (cond
                      (wall? xy)      "#"
                      (= entrance xy) "@"
                      (= exit xy)     "*"
                      (path? xy)      "."
                      :else           " "))
        xs       (range 1 (inc maxx))
        ys       (range 1 (inc maxy))
        render   (fn [path]
                   (->> ys
                     (map (fn [y]
                            (->> xs
                              (map (fn [x] (cell (set path) [x y])))
                              (str/join)
                              (str y))))
                     (str/join "\n")
                     (str " " (str/join (map unparse-x xs)) "\n")
                     (println)))
        opts     (memoize
                   (fn [[x y]]
                     (let [x- (dec x), x+ (inc x)
                           y- (dec y), y+ (inc y)]
                       (->>
                         [(when (<= 1 y-)    [x  y-])  ;;up
                          (when (<= 1 x-)    [x- y])   ;;left
                          (when (<= x+ maxx) [x+ y])   ;;right
                          (when (<= y+ maxy) [x  y+])];;down
                         (remove nil?)
                         (remove wall?)))))]
    ;(println "maze:")
    ;(render #{})
    (loop [todo     [[#{} 1 [entrance]]]
           shortest nil
           longest  nil]
      (blet [[seen pathlen path] (peek todo)
             xy        (peek path)
             todo-     (pop todo)
             seen+     (conj seen xy)
             xys       (->> xy opts (remove seen))
             pathlen+  (inc pathlen)
             todos     (map (fn [xy] [seen+ pathlen+ (conj path xy)]) xys)
             todo+     (into todo- todos)
             shortlen  (if shortest (count shortest) Integer/MAX_VALUE)
             longlen   (if longest (count longest) 0)
             shortest* (if (< pathlen shortlen) path shortest)
             longest*  (if (< longlen pathlen) path longest)]
        (cond
          (empty? todo) (do
                          ;(println "shortest:")
                          ;(render shortest)
                          ;(println "longest:")
                          ;(render longest)
                          [shortest longest])
          ;(< shortlen pathlen) (recur todo- shortest longest)
          (= exit xy)   (recur todo- shortest* longest*)
          (empty? xys)  (recur todo- shortest longest)
          :else         (recur todo+ shortest longest))))))


(time (solve INPUT))
#_(time (dotimes [_ 1000] (solve INPUT)))


;maze:
; ABCDEFGH
;1@ #   #
;2# # # #
;3  # #
;4 ## ###
;5    #
;6 # ## ##
;7 # #  #
;8 #     *
;shortest:
; ABCDEFGH
;1@.#   #
;2#.# # #
;3..# #
;4.## ###
;5... #
;6 #.## ##
;7 #.#  #
;8 #.....*
;longest:
; ABCDEFGH
;1@.#...#
;2#.#.#.#
;3..#.#...
;4.##.###.
;5....#...
;6 # ##.##
;7 # #..#
;8 #  ...*
;"Elapsed time: 5.086715 msecs"