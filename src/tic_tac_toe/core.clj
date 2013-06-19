(ns tic-tac-toe.core
  (:gen-class))

(require '[clj-http.client :as client])

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
  (will make this less hard-coded in the future. Maybe.)"
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

(defn rand-set-el
  "Returns a random element from a set"
  [s]
  ((vec s) (rand-int (count s))))

(defn fill-random-square
  "returns a grid with a random square filled
  (for testing only, before AI is created)"
  [grid marker]
  (assoc-in grid 
            (rand-set-el (free-squares grid))
            marker))

(defn opponent-marker [marker]
  (case marker
    "X" "O"
    "O" "X"))

(defn pick-square-heuristic
  "picks the best square based on a series of priorities"
  [grid marker]
  (let [free (free-squares grid)
        other-two-squares-filled (fn [m]
                                   (set (filter 
                                          (fn [free-square]
                                            (some (fn [three-squares]
                                                    (every? #(= (get-in grid %) m) 
                                                            (disj three-squares free-square)))
                                                  three-squares-in-a-row-sets))
                                          free)))
        winning (other-two-squares-filled marker)
        losing (other-two-squares-filled (opponent-marker marker))
        center ('clojure.set/difference #{[1 1]} free)
        corners ('clojure.set/difference #{[0 0][0 2][2 0][2 2]} free)]
    (cond
      (seq? winning) (rand-set-el winning)
      (seq? losing) (rand-set-el losing)
      (seq? center) (rand-set-el center)
      (seq? corners) (rand-set-el corners)
      (seq? free) (rand-set-el free)
      :else nil)))


(defn game-over?
  [grid]
  (some (fn [three-squares]
          (let [all-one-marker (fn [marker]
                                 (every? #(= (get-in grid %) marker) 
                                         three-squares))]
            (or (all-one-marker "X") (all-one-marker "O"))))
        three-squares-in-a-row-sets))

(defn make-it-so []
  (loop [grid (empty-grid)
         marker "X"]
    (do (print-board grid))
    (if (game-over? grid)
      (str "Game over! " (opponent-marker marker) " won.")
      (recur (fill-random-square grid marker)
             (opponent-marker marker)))))

; Priorities:

; 1) Win
; 2) Prevent disaster
; 3) Put it in the middle
; 4) Put it in the corner
; 5) Put it anywhere