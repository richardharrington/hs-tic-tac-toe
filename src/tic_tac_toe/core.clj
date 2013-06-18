(ns tic-tac-toe.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, Worldddd!"))


(defn matrix-transpose
  "transposes a 2d matrix"
  [mtx]
  (vec (apply map vector mtx)))

(defn vector-add
  "vector addition"
  [& vecs]
  (vec (apply map + vecs)))

(defn scalar-vector-mult
  "vector-scalar multiplication"
  [s v]
  (vec (map #(* s %) v)))

(defn three-squares-in-a-row
  "lines of three squares in a row, taken from 
  a set of coordinates, and a direction (vector) 
  to extend it in. Input must be well-formed,
  meaning there have to be three squares in that direction."
  [coordinates direction]
  (set (map #(vector-add (scalar-vector-mult % direction)
                         coordinates)
            (range 3))))

(def three-squares-in-a-row-sets
  "all the sets of three squares in a row
  (will make this less hard-coded in the future. Maybe."
  #{(three-squares-in-a-row [0 0] [0 1])
    (three-squares-in-a-row [1 0] [0 1])
    (three-squares-in-a-row [2 0] [0 1])
    (three-squares-in-a-row [0 0] [1 0])
    (three-squares-in-a-row [0 1] [1 0])
    (three-squares-in-a-row [0 2] [1 0])
    (three-squares-in-a-row [0 0] [1 1])
    (three-squares-in-a-row [2 0] [-1 1])})


(defn coordinates-set
  "returns a set of coordinate pairs for a 2d vector"
  [v]
  (set (for [i (range (count v))
             j (range (count (v i)))]
         [i j])))

(defn empty-grid
  "returns an empty grid to start the game"
  []
  (vec (repeat 3 (vec (repeat 3 "*")))))
  
(defn free-squares
  "returns a set of coordinate pairs of free squares on the board"
  [grid]
  (set (filter #(= (get-in grid %) "*") 
               (coordinates-set grid))))

(defn interpose-bounding
  "like interpose, but also adds the divider
   at the beginning and the end"
   [divider s]
   (concat (interleave (repeat divider) s) [divider]))

(defn output-board
  "returns a string for displaying the board"
  [grid]
  (let [interpose-bounding (fn [divider s]
                             (concat (interleave (repeat divider) s) 
                                     [divider]))]
    (apply str (interpose-bounding 
                 "+---+---+---+\n" 
                 (map (fn [row] 
                        (apply str (concat (interpose-bounding 
                                             "|" 
                                             (map #(str " " % " ") row)) "\n")))
                      (matrix-transpose grid))))))
 

(def print-board #(println (output-board %)))

(defn rand-vec-el
  "Returns a random element from a vector"
  [v]
  (v (rand-int (count v))))

(defn fill-random-square
  "returns a grid with a random square filled"
  [grid marker]
  (assoc-in grid 
                  (rand-vec-el (vec (free-squares grid))) 
                  marker))


; (defn winning-squares
;   "Returns coordinates for all the squares that will
;   give an immediate win"
;   [grid marker]
;   )

(defn game-over?
  [grid]
  (some (fn [three-squares]
          (let [all-one-marker (fn [marker]
                                 (every? #(= (get-in grid %) marker) 
                                         three-squares))]
            (or (all-one-marker "X") (all-one-marker"O"))))
        three-squares-in-a-row-sets))

(defn make-it-so []
  (loop [grid (empty-grid)
         marker "X"]
    (do (print-board grid))
    (cond
      (game-over? grid) nil
      :else (recur (fill-random-square grid marker)
                   (if (= marker "X") "O" "X")))))

; Priorities:

; 1) Win
; 2) Prevent disaster
; 3) Put it in the middle
; 4) Put it in the corner
; 5) Put it anywhere