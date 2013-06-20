(ns tic-tac-toe.core
  (:gen-class))

(require '[clj-http.client :as client])
(require 'clojure.set)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false)))


(defn matrix-transpose
  "transposes a 2d matrix"
  [mtx]
  (vec (apply map vector mtx)))

(defn rand-seq-el
  "Returns a random element from a sequence (or nil if it's empty)"
  [s]
  (if (empty? s) nil
    ((vec s) (rand-int (count s)))))   
   
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

(def three-squares-in-a-row-sets
  "all the sets of three squares in a row"
  (clojure.set/union 
    ; vertical wins
    (set (for [x (range 3)] 
           (set (for [y (range 3)] 
                  [x y]))))
    ; horizontal wins
    (set (for [y (range 3)] 
           (set (for [x (range 3)] 
                  [x y]))))
    ; diagonal wins
    #{(set (for [i (range 3)]
             [i i]))
      (set (for [i (range 3)]
             [i (- 2 i)]))}))

(defn winner
  "returns 'X', 'O', or nil"
  [grid]
  (some (fn [three-squares]
          (let [all-one-marker (fn [marker]
                                 (when (every? #(= (get-in grid %) marker) 
                                         three-squares)
                                   marker))]
            (or (all-one-marker "X") (all-one-marker "O"))))
        three-squares-in-a-row-sets))
    
(defn board-full?
  [grid]
  (not-any? (fn [column] 
              (some #(= % "*") column))
            grid))

(defn interpose-bounding 
  "helper function for output-board,
  like interpose but adds on either end as well"
  [divider s]
  (concat (interleave (repeat divider) s) 
          [divider]))

(defn output-board
  "returns a string for displaying the board"
  [grid]
  (apply str (interpose-bounding 
               "+---+---+---+\n" 
               (map (fn [row] 
                      (apply str (concat (interpose-bounding 
                                           "|" 
                                           (map #(str " " % " ") row)) "\n")))
                    (matrix-transpose grid)))))
 

(def print-board #(println (output-board %)))

(defn fill-random-square
  "returns a grid with a random square filled
  (for testing only, before AI is created)"
  [grid marker]
  (assoc-in grid 
            (rand-seq-el (free-squares grid))
            marker))

(defn opposite-marker [marker]
  (case marker
    "X" "O"
    "O" "X"))

(declare value-of-square-minimax)

(defn aggregate-value-of-free-squares-minimax
  "helper function to use recursively with value-of-square-minimax"
  [grid marker]
  (apply (if (= marker "X") max min)
         (map (fn [square]
                (value-of-square-minimax grid square marker))
              (free-squares grid))))

(defn value-of-square-minimax
  "determines the value of a square:
  1 is an eventual win for X, 0 is a draw,
  -1 is an eventual win for O."
  [grid square marker]
  (let [potential-grid (assoc-in grid square marker)
        winning-player (winner potential-grid)]
    (cond
      (= winning-player "X") 1
      (= winning-player "O") -1
      (board-full? potential-grid) 0
      :else (aggregate-value-of-free-squares-minimax 
              potential-grid 
              (opposite-marker marker)))))

(defn pick-square-from-result-set
  "helper function for pick-square-minimax"
  [result-sets marker]
  (cond
    (= marker "X") (or (rand-seq-el (result-sets 1))
                       (rand-seq-el (result-sets 0))
                       (rand-seq-el (result-sets -1)))
    (= marker "O") (or (rand-seq-el (result-sets -1))
                       (rand-seq-el (result-sets 0))
                       (rand-seq-el (result-sets 1)))))

(defn pick-square-minimax
  "picks a random (but good) square using the minimax algorithm"
  [grid marker]
  (let [result-sets (reduce 
                      (fn [result-sets new-square]
                        (let [val-of-square (value-of-square-minimax
                                              grid
                                              new-square
                                              marker)]
                          (assoc 
                            result-sets 
                            val-of-square 
                            (conj (result-sets val-of-square) new-square))))
                      {1 #{}, -1 #{}, 0 #{}}
                      (free-squares grid))]
    (pick-square-from-result-set result-sets marker)))

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
        (rand-seq-el (imminent-win (opposite-marker marker)))
        (rand-seq-el (clojure.set/intersection #{[1 1]} free))
        (rand-seq-el (clojure.set/intersection #{[0 0][0 2][2 0][2 2]} free))
        (rand-seq-el free))))

(defn get-next-grid
  "gets a new game state with one marker added"
  [picker grid marker]
  (assoc-in grid
            (picker grid marker)
            marker))

(defn go []
  (loop [grid (empty-grid)
         marker "X"
         picker pick-square-minimax]
    (let [opposite-picker (fn [picker]
                            (cond
                              (= picker pick-square-heuristic) pick-square-minimax
                              (= picker pick-square-minimax) pick-square-heuristic))]
      (do (print-board grid))
      (let [winning-player (winner grid)]
        (cond
          winning-player (str "Game over! " winning-player " won.")
          (board-full? grid) "Game over! Draw."
          :else (recur (get-next-grid picker grid marker)
                       (opposite-marker marker)
                       picker))))))
    


  
