(ns tic-tac-toe.core
  (:gen-class))

(require '[clj-http.client :as client])
(require 'clojure.set)
(require '[cheshire.core :as json])

(def server-path "http://localhost:5000")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false)))

(defn toggler-maker
  "makes a function that toggles two things"
  [thing-1 thing-2]
  #(condp = %
     thing-1 thing-2
     thing-2 thing-1))

(def import-map {"X" 1, "O" -1, " " 0})
(def export-map (clojure.set/map-invert import-map))
                 
(defn import-grid
  "for testing only, so you can put in vectors of rows with Xs, Os and spaces"
  [grid-template]
  (map #(import-map %) (flatten grid-template)))
  
(defn export-grid-for-display
  "for printing"
  [grid]
  (partition 3 (map #(export-map %) grid)))

(defn rand-seq-el
  "Returns a random element from a sequence (or nil if it's empty)"
  [s]
  (if (empty? s) nil
    ((vec s) (rand-int (count s)))))   
   
(defn coordinates-set
  "returns a set of coordinate pairs for a given size square grid"
  [size]
  (set (for [i (range size)
             j (range size)]
         [i j])))

(defn get-marker
  "takes a 3x3 data structure represented as a flat vector
  of length 9, and returns a marker at an x-y coordinate"
  [grid [x y]]
  (grid (+ (* y 3) x)))

(defn assoc-marker
  "returns a flat vector of length 9, representing a 3x3 data
  structure with a marker updated at position x, y"
  [grid [x y] marker]
  (assoc grid (+ (* y 3) x) marker))

(def empty-grid (vec (repeat 9 0)))

(defn valid-grid
  "prevents null pointer exceptions 
   if a function needs to call a grid and it's nil"
  [grid]
  (or grid empty-grid))
  
(defn free-squares
  "returns a set of coordinate pairs of free squares on the board"
  [grid]
  (set (filter #(= (get-marker grid %) 0) 
               (coordinates-set 3))))

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
                    (export-grid-for-display grid)))))
 

(def print-board #(println (output-board %)))

(defn board-full?
  [grid]
  (not-any? #{0} grid))

(defn final-score
  "returns 1 for X, -1 for O, 0 for draw, and nil if the game is not over 
  yet (needs well-formed input: no more than one winning sequence,
  because it returns the first streak of 1s or -1s that it finds).
  TODO: Get someone to show me how to do this better, without switching
  back and forth between 0 and nil all the time."
  [grid]
  (let [grid (valid-grid grid)]
    (or
      (some (fn [three-squares]
              (let [val (quot (apply + (map #(get-marker grid %) three-squares)) 3)]
                (if (= val 0) 
                  nil 
                  val)))
            three-squares-in-a-row-sets)
      (if (board-full? grid) 
        0 
        nil))))

(defn pick-square-random
  "returns a grid with a random square filled
  (for testing only, so that we can see results when players win and lose).
  Extra parameter is because all other picking functions need to know which marker."
  [grid _]
  (rand-seq-el (free-squares grid)))

(declare value-of-square-minimax)

(defn aggregate-value-of-free-squares-minimax
  "helper function to use recursively with value-of-square-minimax"
  [grid marker]
  (apply (if (= marker 1) max min)
         (map (fn [square]
                (value-of-square-minimax grid square marker))
              (free-squares grid))))

(def value-of-square-minimax
  "memoized function which determines the value of a square:
  1 is an eventual win for X, 0 is a draw,
  -1 is an eventual win for O."
  (memoize 
    (fn [grid square marker]
      (let [potential-grid (assoc-marker grid square marker)]
        (or (final-score potential-grid)
            (aggregate-value-of-free-squares-minimax 
              potential-grid 
              (- marker)))))))

(defn pick-square-from-result-set
  "helper function for pick-square-minimax"
  [result-sets marker]
  (or (rand-seq-el (result-sets marker))
      (rand-seq-el (result-sets 0))
      (rand-seq-el (result-sets (- marker)))))

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
                                        (every? #(= (get-marker grid %) m) 
                                                (disj three-squares free-square)))
                                      three-squares-in-a-row-sets))
                              free)))]
    
    (or (rand-seq-el (imminent-win marker))
        (rand-seq-el (imminent-win (- marker)))
        (rand-seq-el (clojure.set/intersection #{[1 1]} free))
        (rand-seq-el (clojure.set/intersection #{[0 0][0 2][2 0][2 2]} free))
        (rand-seq-el free))))

(defn get-next-grid
  "gets a new game state with one marker added"
  [picker grid marker]
  (assoc-marker grid
                (picker (valid-grid grid) marker)
                marker))
   
(defn final-message
  [score]
  (case score
    (1 -1) (str "Game over! " (export-map score) " won." )
    0 "Game over! Draw."))

; Beginning of the section for local play only.

(defn play-game-local [player1-picker player2-picker]
  "A function for playing a game with two local
   pickers against each other.
   TODO: Integrate this with the playing-against-the-server stuff"
  (let [opposite-picker 
        (toggler-maker player1-picker player2-picker)]
    (loop [grid empty-grid
           marker 1
           picker player1-picker]
      (do (print-board grid))
      (let [score (final-score grid)]
        (if score
          (final-message score)
          (recur (get-next-grid picker grid marker)
                 (- marker)
                 (opposite-picker picker)))))))

; Beginning of the section related to playing a game with the server.

(defn ask-server [player-id]
  "Checks the server and returns result to poll-server below,
   where the looping is done"
  (Thread/sleep 50)
  (let [{{status :status board :board} :body}
            (client/get (str server-path "/get_board/" player-id) {:as :json})]
    (if (= status "hold tight")
      [false (board :board)]
      [true board])))
 
(defn send-server [board player-id]
  (client/post (str server-path "/submit_board/" player-id) 
               {:form-params {:data (json/generate-string {:board board})}}))

; (def print-waiting-message
;   "Prints a waiting message, but only once for each board configuration"
;   (memoize (fn [board] (println "Awaiting opponent's move...\n"))))

(defn poll-server 
  "the main loop for playing a game with the server."
  [player-id get-local-ply-cb]
  (loop [[our-turn? board] (ask-server player-id)]
    (let [score (final-score board)]
      (if score
        (final-message score)
        (do
          (when our-turn?
            (print-board board)
            (println "Now making local move...\n")
            (let [new-board (get-local-ply-cb board)]
              (print-board new-board)
              (send-server new-board player-id)))
          (recur (ask-server player-id)))))))
        


(defn play-request
  "requests a new game from the server, and calls poll-server with
  different parameters depending on whether we're player 1 or player 2
  (which is determined by a slightly convoluted test, because of the server API)"
  [local-picker]
  (println "\nWelcome to tic-tac-toe")
  (let [{{board :board, player1-id :player1, player2-id :player2} :body} 
        (client/get (str server-path "/play_request") {:as :json})]
    (cond 
      ; if we didn't get a board, or we got a board
      ; with one square already filled, then we're player 2.
      (or (nil? board) (= (apply + board) 1))
        (do 
          (println "This computer will be playing Os\n with id" player2-id)
          (poll-server player2-id (fn [board] (get-next-grid local-picker board -1))))
      :else 
        (do 
          (println "This computer will be playing Xs\n with id" player1-id)
          (poll-server player1-id (fn [board] (get-next-grid local-picker board 1)))))))


; Testing functions

(defn go
  "Function with a conveniently short name, for testing"
  []
  (play-request pick-square-minimax))

(defn test-full-board-winning
  "Function to test whether a full board can correctly score as winning"
  []
  (loop [grid nil]
    (let [final (final-score grid)
          full (board-full? grid)]
      (if (and final (not= final 0) full)
        (str (final-score grid) " won.")
        (recur (play-game-local pick-square-heuristic pick-square-random))))))





