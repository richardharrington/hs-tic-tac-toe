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

(def import-grid-template 
  "converts vector of rows (easier to type in for tests)
  into vector of columns (easier to deal with programmatically,
  because it's [x y] instead of [y x]"
  matrix-transpose)

; (defn replace-item-nested-2d
;   "returns a 2d vector with one item replaced at coordinates"
;   [v val i j]
;   (assoc v i (assoc (v i) j val)))

(def export-grid-for-display
  "transposes vector-of-columns to vector-of-rows for display"
  matrix-transpose)

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
  
(def marker-at 
  "gets the value (either 'O', 'X', or '*' for free) of a square in a 2d 3x3 grid"
  get-in)

(def replace-marker 
  "replaces a marker at the specified coordinates and returns the whole nested structure"
  assoc-in)

(defn free-squares
  "returns a set of coordinate pairs of free squares on the board"
  [grid]
  (set (filter #(= (marker-at grid %) "*") 
               (coordinates-set grid))))

(defn interpose-bounding
  "like interpose, but also adds the divider
   at the beginning and the end"
   [divider s]
   (concat (interleave (repeat divider) s) [divider]))

(defn output-board
  "returns a string for displaying the board"
  [grid]
  (apply 
    str (interpose-bounding "+---+---+---+\n" 
                            (map (fn [row] 
                                   (apply 
                                     str (concat (interpose-bounding 
                                                   "|" 
                                                   (map #(str " " % " ") row)) "\n")))
                                 (export-grid-for-display grid)))))

(def print-board #(println (output-board %)))

(defn rand-vec-el
  "Returns a random element from a vector"
  [v]
  (v (rand-int (count v))))
                             

(defn fill-random-square
  "returns a grid with a random square filled"
  [grid marker]
  (replace-marker grid 
                  (rand-vec-el (vec (free-squares grid))) 
                  marker))
   
; display

; (def print-board
;     "prints the board from a grid"
;     [grid]
;     (let [display-grid (export-grid-for-display grid)]
;          (println (str "-------\n" ))

