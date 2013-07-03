(ns tic-tac-toe.core
  (:require [clj-http.client :as client]
            clojure.set
            [cheshire.core :as json]))

(def server-path "http://thomasballinger.com:8001/")

; to run locally, clone the epo for the tictax server from
; https://github.com/eriktaubeneck/tictax,
; install python and Flask, run the server, and change the pathname above to
; "http://localhost:5000/"


(def empty-board (vec (repeat 9 0)))

(defn valid-board
  "prevents null pointer exceptions 
   if a function needs to call a board and it's nil"
  [board]
  (or board empty-board))
  
(defn free-squares
  "returns a set of indexes of free squares on the board"
  [board]
  (set (filter #(zero? (board %)) (range 9))))

(defn board-full?
  [board]
  (not-any? zero? board))

(defn score
  "Given a board, return:
  nil -- game not over
  1   -- X wins 
  0   -- tie
  -1  -- O wins"
[board]
(let [rows (partition 3 board)
      cols (apply map vector rows)
      diags (for [diag [[0 4 8] [2 4 6]]]
              (map board diag))
      winner (some (fn [line]
                     (case (apply + line)
                       -3 -1
                       3 1
                       nil))
                   (concat rows cols diags))]
  (or winner (when (board-full? board) 0))))

(defn imminent-wins [board marker]
  (filter (fn [square]
            (= (score (assoc board square marker)) marker))
          (free-squares board)))


; Now for the picking functions         
          
(defn rand-seq-el
  "Returns a random element from a sequence (or nil if it's empty)"
  [s]
  (if (empty? s) nil
    ((vec s) (rand-int (count s))))) 

(defn pick-square-random
  "returns a board with a random square filled
  (for testing only, so that we can see results when players win and lose).
  Extra parameter is because all other picking functions need to know which marker."
  [board _]
  (rand-seq-el (free-squares board)))

; User input picking

(defn valid-int-string [n]
  (re-find #"^-?\d+$" n))

(defn get-word-sequence [s]
  (re-seq #"\S+" s))

(defn convert-seq-to-int [s]
  (if (every? valid-int-string s)
    (map #(Integer/parseInt %) s)
    nil))

(defn pick-square-from-user-input
  "Get coordinates from user and see whether they
  a) are 2 in number
  b) fall within the correct bounds
  c) represent a free square
  (unused parameter is there because other picking methods need it)"
  [board _]
  (loop []
    (println "Type in a coordinate pair (like '1 3' for 1 over and 3 down).")
    (let [coords-idx-from-1 (convert-seq-to-int (get-word-sequence (read-line)))
          coords (and coords-idx-from-1 (map dec coords-idx-from-1))]
      (if (and coords
               (= (count coords) 2)
               (every? #(<= 0 % 2) coords)
               ((free-squares board) coords))
        (+ (coords 0) (* (coords 1) 3))
        (do
          (println "Your typing might have been a little off. Try again.")
          (recur))))))

(declare value-of-square-minimax)

; CODE REVIEW WITH ALEX: This might be slow because of a lot of implicit
; (or explicit) sequence conversions (vector to map; etc.)
; TOOL: Visual VM

(defn aggregate-value-of-free-squares-minimax
  "helper function to use recursively with value-of-square-minimax"
  [board marker]
  (apply (if (= marker 1) max min)
         (map (fn [square]
                (value-of-square-minimax board square marker))
              (free-squares board))))

(def value-of-square-minimax
  "memoized function which determines the value of a square:
  1 is an eventual win for X, 0 is a tie,
  -1 is an eventual win for O."
  (memoize 
    (fn [board square marker]
      (let [potential-board (assoc board square marker)]
        (or (score potential-board)
            (aggregate-value-of-free-squares-minimax 
              potential-board 
              (- marker)))))))

(defn pick-square-from-result-set
  "helper function for pick-square-minimax"
  [result-sets marker]
  (or (rand-seq-el (result-sets marker))
      (rand-seq-el (result-sets 0))
      (rand-seq-el (result-sets (- marker)))))

(defn pick-square-minimax
  "makes three sets of squares (winning, drawing and losing)
   and picks at random from the best of them."
  [board marker]
  (let [result-sets (reduce 
                      (fn [result-sets new-square]
                        (let [val-of-square (value-of-square-minimax
                                              board
                                              new-square
                                              marker)]
                          (assoc 
                            result-sets 
                            val-of-square 
                            (conj (result-sets val-of-square) new-square))))
                      {1 #{}, -1 #{}, 0 #{}}
                      (free-squares board))]
    (pick-square-from-result-set result-sets marker)))

(defn pick-square-heuristic
  "picks the best square based on a series of priorities:
  1) Win, 2) Prevent imminent loss, 3) Center, 4) Corner, 5) Anywhere"
[board marker]
(println (imminent-wins board marker))
(println (imminent-wins board (- marker)))
(or (rand-seq-el (imminent-wins board marker))
    (rand-seq-el (imminent-wins board (- marker)))
    (rand-seq-el (clojure.set/intersection #{4} (free-squares board)))
    (rand-seq-el (clojure.set/intersection #{0 2 6 8} (free-squares board)))
    (rand-seq-el (free-squares board))))

(defn get-next-board
  "gets a new game state with one marker added"
  [picker board marker]
  (assoc board (picker (valid-board board) marker) marker))


; Now for all the code that actually plays and outputs games

(def export-map {1 "X", -1 "O", 0 " "})

(defn export-board-for-display
  "for printing"
  [board]
  (partition 3 (map #(export-map %) board)))

(defn interpose-bounding 
  "helper function for output-board,
  like interpose but adds on either end as well"
  [divider s]
  (concat (interleave (repeat divider) s) 
          [divider]))

(defn output-board
  "returns a string for displaying the board"
  [board]
  (apply str (interpose-bounding 
               "+---+---+---+\n" 
               (map (fn [row] 
                      (apply str (concat (interpose-bounding 
                                           "|" 
                                           (map #(str " " % " ") row)) "\n")))
                    (export-board-for-display board)))))

(def print-board #(println (output-board %)))
   
(defn final-message
  [score]
  (case score
    (1 -1) (str "Game over! " (export-map score) " won.\n" )
    0 "Game over! Draw.\n"))

; Beginning of the section for local play only.

(defn play-game-local [player1-picker player2-picker]
  "A function for playing a game with two local
   pickers against each other.
   TODO: Integrate this with the playing-against-the-server stuff"
  (let [opposite-picker {player1-picker player2-picker
                         player2-picker player1-picker}]
    
    (loop [board empty-board
           marker 1
           picker player1-picker]
      (print-board board)
      (let [scr (score board)]
        (if scr
          (do 
            (println (final-message scr))
            board)
          (recur (get-next-board picker board marker)
                 (- marker)
                 (opposite-picker picker)))))))

; Beginning of the section related to playing a game with the server.

(defn ask-server [player-id]
  "Checks the server and returns result to poll-server below,
   where the looping is done"
  (Thread/sleep 50)
  (let [{{status :status board :board} :body}
            (client/get (str server-path "get_board/" player-id) {:as :json})]
    (if (= status "hold tight")
      [false (board :board)]
      [true board])))
 
(defn send-server [board player-id]
  (client/post (str server-path "submit_board/" player-id) 
               {:form-params {:data (json/generate-string {:board board})}}))

; (def print-waiting-message
;   "Prints a waiting message, but only once for each board configuration"
;   (memoize (fn [board] (println "Awaiting opponent's move...\n"))))

(defn poll-server 
  "the main loop for playing a game with the server."
  [player-id get-local-ply-cb]
  (loop [[our-turn? board] (ask-server player-id)]
    (let [scr (score board)]
      (if scr
        (do
          (when our-turn? (print-board board))
          (println (final-message scr)))
        (do
          (when our-turn?
            (print-board board)
            (println "Now making local move...\n")
            (let [new-board (get-local-ply-cb board)]
              (print-board new-board)
              (when (not (score new-board))
                (println "Awaiting opponent's move...\n"))
              (send-server new-board player-id)))
          (recur (ask-server player-id)))))))
        


(defn play-request
  "requests a new game from the server, and calls poll-server with
  different parameters depending on whether we're player 1 or player 2
  (which is determined by a slightly convoluted test, because of the server API)"
  [local-picker]
  (println "\nWelcome to tic-tac-toe")
  (let [{{board :board, player1-id :player1, player2-id :player2} :body} 
        (client/get (str server-path "play_request") {:as :json})]
    (cond 
      ; if we didn't get a board, or we got a board
      ; with one square already filled, then we're player 2.
      (or (nil? board) (= (apply + board) 1))
        (do 
          (println "This computer will be playing Os with id" player2-id "\n")
          (poll-server player2-id (fn [board] (get-next-board local-picker board -1))))
      :else 
        (do 
          (println "This computer will be playing Xs with id" player1-id "\n")
          (poll-server player1-id (fn [board] (get-next-board local-picker board 1)))))))


; Testing functions

(defn go
  "Function with a conveniently short name, for testing"
  []
  (play-request pick-square-minimax))

