(ns tic-tac-toe.core
  (:gen-class))

(require '[clj-http.client :as client])
(require 'clojure.set)

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

(defn rand-seq-el
  "Returns a random element from a sequence (or nil if it's empty)"
  [s]
  (if (empty? s) nil
    ((vec s) (rand-int (count s)))))

(defn fill-random-square
  "returns a grid with a random square filled
  (for testing only, before AI is created)"
  [grid marker]
  (assoc-in grid 
            (rand-seq-el (free-squares grid))
            marker))

(defn opponent-marker [marker]
  (case marker
    "X" "O"
    "O" "X"))

; (defn pick-square-minimax
;   "plays out the game to all its possible conclusions
;   and works back to find the best set of moves"
;   [grid marker]
;   (let [winning-player (winner grid)]
;     (cond
;       (= winning-player marker) 1
;       (= winning-player (opponent-marker) -1)
;       (board-full? 0)
;       :else (pick-square-minimax )
      

(defn pick-square-heuristic
  "picks the best square based on a series of priorities:
   1) Win, 2) Prevent imminent loss, 3) Center, 4) Corner, 5) Anywhere"
  [grid marker]
  (let [free (free-squares grid)
        imminent-win (fn [m]
                       (set (filter 
                              (fn [free-square]
                                (some (fn [three-squares]
                                        (every? #(= (get-in grid %) m) 
                                                (disj three-squares free-square)))
                                      three-squares-in-a-row-sets))
                              free)))]
    
    (or (rand-seq-el (imminent-win marker))
        (rand-seq-el (imminent-win (opponent-marker marker)))
        (rand-seq-el (clojure.set/intersection #{[1 1]} free))
        (rand-seq-el (clojure.set/intersection #{[0 0][0 2][2 0][2 2]} free))
        (rand-seq-el free))))
  
(defn winner
  "Returns 'X', 'O' or nil. Returns the marker of the first
   three-in-a-row it finds, so board must be valid
   (i.e., no two sets of three in a row"
  [grid]
  
  (let [marker-all-three (fn [three-squares]
                           (if every? #(= (get-in grid %) 
                                          three-squares))])
  
  (let [all-one-marker? (fn [] (every?  = %)]
    (some (fn [three-squares]
            (some all-in-one-marker? ))
          (three-squares-in-a-row-sets))
        
        (fn [three-squares]
                          (apply = ))])
  
  (let [all-one-marker? (fn [three-squares marker]
                         (every? #(= (get-in grid %) marker) 
                                 three-squares))
        any-threes-filled? (fn [marker]
                             (some #(all-one-marker? % marker)
                                   three-squares-in-a-row-sets))]
    (cond
      (any-threes-filled? "X") "X"
      (any-threes-filled? "O") "O"
      :else nil)))
    
(defn board-full?
  [grid]
  (not-any? (fn [column] 
              (some #(= % "*") column))
            grid))

; for debugging: (def pick-square-heuristic fill-random-square)

(defn make-it-so []
  (loop [grid (empty-grid)
         marker "X"]
    (do (print-board grid))
    (let [winning-player (winner grid)]
      (cond
        winning-player (str "Game over! " winning-player " won.")
        (board-full? grid) "Game over! Draw."
        :else (recur (fill-random-square grid marker)
                     (opponent-marker marker))))))
  
