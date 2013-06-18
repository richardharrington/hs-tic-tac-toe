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

(defn three-squares-in-a-row
  "returns a set of coordinates for a three-square line
   given one square and a direction vector (assumes
   well-formed input, i.e. there exist three squares
   in that direction)"
   [coordinates direction]
   (set (map #(vector-add (scalar-vector-mult % direction)
                          coordinates)
             (range 3))))

(defn three-squares-in-a-row-sets
  "returns all the sets of three squares in a row
  (will make this less hard-coded in the future. Maybe."
  []
  #{(three-squares-in-a-row [0 0] [0 1])
    (three-squares-in-a-row [1 0] [0 1])
    (three-squares-in-a-row [2 0] [0 1])
    (three-squares-in-a-row [0 0] [1 0])
    (three-squares-in-a-row [0 1] [1 0])
    (three-squares-in-a-row [0 2] [1 0])
    (three-squares-in-a-row [0 0] [1 1])
    (three-squares-in-a-row [2 0] [-1 1])})

(defn game-over?
  "fix this; it's repeating code"
  [grid]
  (some (fn [three-squares]
          (or (every? #(let [marker (get-in grid %)]
                     (= "X" marker))
                  three-squares)
              (every? #(let [marker (get-in grid %)]
                     (= "O" marker))
                  three-squares)))
        (three-squares-in-a-row-sets)))

(defn make-it-so []
  (loop [grid (empty-grid)
         marker "X"]
    (do (print-board grid))
    (cond
      (game-over? grid) nil
      :else (recur (fill-random-square grid marker)
                   (if (= marker "X") "O" "X")))))