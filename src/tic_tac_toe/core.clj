(ns tic-tac-toe.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


; grid: [ ["*" "*" "X"] 
;         ["*" "*" "*"]
;         ["*" "*" "*"] ]

; square: [0 1]

(defn matrix-transpose
    "transposes a 2d matrix"
    [mtx]
    (vec (apply map vector mtx)))

; import-grid-template and export-grid-template
; are so that we can 
; convert our matrices from a vector of rows
; (more intuitive to type in and display)
; to a vector of columns (more intuitive to work
; with programmatically, because it's x, y)

(def import-grid-template matrix-transpose)
(def export-grid-to-display matrix-transpose)

(defn replace-item
    "returns a vector with one item replaced at index value"
    [v idx val]
    (concat (take idx v) [val] (drop (inc idx) v)))

(defn replace-item-nested-2d
    "returns a 2d vector with one item replaced at coordinates"
    [v i j val]
    (replace-item v i (replace-item (v i) j val)))

(defn marker-at 
    "gets the value (either 'O', 'X', or '*' for free) of a square in a 2d 3x3 grid"
    [grid x y]
    ((grid x) y))

(def add-marker replace-item-nested-2d)

