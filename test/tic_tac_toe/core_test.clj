(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(def test-grid-template
  [["X" "O" "X"] 
   ["O" "*" "O"]
   ["O" "*" "O"]])

(def test-grid
  [["X" "O" "O"]
   ["O" "*" "*"]
   ["X" "O" "O"]])

(def winner-test-grid
  [["X" "X" "X"]
   ["O" "*" "*"]
   ["X" "O" "O"]])

(def board-full-grid
  [["X" "X" "X"]
   ["O" "O" "X"]
   ["X" "O" "O"]])

(deftest matrix-transpose-test
  (testing "transposing a board from organized by rows to organized by columns"
    (is (= (matrix-transpose test-grid-template)
           test-grid))))

(deftest three-squares-in-a-row-sets-test
  (testing "returns a set of sets of three points in a line"
    (is (= three-squares-in-a-row-sets
           #{#{[2 1] [2 2] [2 0]} 
             #{[1 0] [0 0] [2 0]} 
             #{[2 1] [1 1] [0 1]} 
             #{[1 0] [1 1] [1 2]} 
             #{[1 1] [0 2] [2 0]} 
             #{[2 2] [0 0] [1 1]} 
             #{[0 0] [0 1] [0 2]} 
             #{[2 2] [1 2] [0 2]}}))))

(deftest coordinates-set-test
  (testing "getting set of coordinates for 2d vector"
    (is (= (coordinates-set [[9 9 8] [3 4 5]])
           #{[0 0] [0 1] [0 2] 
             [1 0] [1 1] [1 2]}))))

(deftest empty-grid-test
  (testing "generating an empty grid"
    (is (= (empty-grid)
           [["*" "*" "*"]
            ["*" "*" "*"]
            ["*" "*" "*"]]))))

(deftest free-squares-test
  (testing "finding free squares"
    (is (= (free-squares test-grid)
           #{[1 1] [1 2]}))))

(deftest interpose-bounding-test
  (testing "interpose-bounding"
    (is (= (interpose-bounding "|" [1 2 3])
           '("|" 1 "|" 2 "|" 3 "|")))))

(deftest output-board-test
  (testing "outputting a string with the state of the board"
    (is (= (output-board test-grid)
           (str "+---+---+---+\n"
                "| X | O | X |\n"
                "+---+---+---+\n"
                "| O | * | O |\n"
                "+---+---+---+\n"
                "| O | * | O |\n"
                "+---+---+---+\n")))))

(deftest fill-random-square-test
  (testing "filling a random square; testing at least whether there is one more of that type of marker"
    (is (= (count (filter #(= % "X") 
                          (flatten (fill-random-square test-grid "X"))))
           3))))

(deftest winner-test-negative-test
  (testing "winner nil"
    (is (= (winner test-grid)
           nil))))

(deftest winner-test-positive-test
  (testing "winner 'X'"
    (is (= (winner winner-test-grid)
           "X"))))

(deftest board-full?-negative-test
  (testing "board is not full"
    (is (not (board-full? test-grid)))))

(deftest board-full?-positive-test
  (testing "board is not full"
    (is (board-full? board-full-grid))))


