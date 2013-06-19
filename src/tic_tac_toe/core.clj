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

(reduce f val col)

(defn opposite-result
  [result]
  (case
    "win" "lose"
    "lose" "win"))

(defn value-of-square-for-X
  [grid square]
  (let [potential-grid (assoc-in grid square "X")
        winning-player (winner potential-grid)]
    (cond
      (= winning-player "X") "win"
      (= winning-player "O") "lose"
      (board-full? potential-grid) "draw"
      :else (opponent-result
              (reduce (fn [result new-square]
                        (let [val (value-of-square-for-O
                                    potential-grid
                                    new-square)]
                          (cond
                            (or (= val "win") (= result "win")) "win"
                            (or (= val "draw") (= result "draw")) "draw"
                            :else "lose")))
                      "lose"
                      (free-squares potential-grid)))))))

(defn value-of-square-for-O
  [grid square])

(defn value-of-square-for-either-player
  [grid square player]
  (let [potential-grid (assoc-in grid square player)
        winning-player (winner potential-grid)]
    (cond 
      (= winning-player player) "win"
      (= winning-player (opponent-marker player)) "lose"
      (board-full? grid) "draw"
      :else (reduce (fn [result square]
                      (value-of-square-no-signs
                        (assoc-in potential-grid square (opponent-marker))
                        square
                        (opponent-marker player)))
                    "lose"
                    (free-squares potential-grid)))))

(defn minimax-value-of-square
  "plays out the game to all its possible conclusions
  and works back to find the best move for that square"
  [grid marker]
  (let [winning-player (winner grid)]
    (cond
      (= winning-player marker) 1
      (= winning-player (opponent-marker) -1)
      (board-full? 0)
      :else (pick-square-minimax ?????)
      

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

(defn pick-new-grid-heuristic
  "picks a good new game state with one marker added"
  [grid marker]
  (assoc-in grid
            (pick-square-heuristic grid marker)
            marker))
  
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

; for debugging: (def pick-square-heuristic fill-random-square)

(defn go []
  (loop [grid (empty-grid)
         marker "X"]
    (do (print-board grid))
    (let [winning-player (winner grid)]
      (cond
        winning-player (str "Game over! " winning-player " won.")
        (board-full? grid) "Game over! Draw."
        :else (recur (pick-new-grid-heuristic grid marker)
                     (opponent-marker marker))))))
  
