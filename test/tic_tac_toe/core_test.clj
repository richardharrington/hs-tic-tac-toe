(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(def test-grid
  [ 1 -1  1
   -1  0 -1
   -1  0 -1 ])

(def test-grid-exported
  [["X" "O" "X"]
   ["O" " " "O"]
   ["O" " " "O"]])

(def winner-test-grid
  [ 1 -1  1
    1  0 -1
    1  0 -1 ])

(def board-full-grid
  [ 1 -1  1
   -1  1 -1
    1  1 -1 ])

(deftest export-grid-for-display-test
  (testing "exporting grid for display"
    (is (= (export-grid-for-display test-grid)
           test-grid-exported))))

(deftest coordinates-set-test
  (testing "getting set of coordinate pairs for 2d square grid"
    (is (= (coordinates-set 3)
           #{[0 0] [0 1] [0 2] 
             [1 0] [1 1] [1 2]
             [2 0] [2 1] [2 2]}))))

(deftest empty-grid-test
  (testing "generating an empty grid"
    (is (= empty-grid
           [0 0 0
            0 0 0
            0 0 0]))))

(deftest free-squares-test
  (testing "finding free squares"
    (is (= (free-squares test-grid)
           #{[1 1] [1 2]}))))

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
                "| O |   | O |\n"
                "+---+---+---+\n"
                "| O |   | O |\n"
                "+---+---+---+\n")))))

(deftest board-full?-negative-test
  (testing "board is not full"
    (is (not (board-full? test-grid)))))

(deftest board-full?-positive-test
  (testing "board is full"
    (is (board-full? board-full-grid))))

(deftest final-score-negative-test
  (testing "final score nil (not over yet)"
    (is (= (final-score test-grid)
           nil))))

(deftest final-score-winner-X-test
  (testing "winner is X (1)"
    (is (= (final-score winner-test-grid)
           1))))

(deftest final-score-draw-test
  (testing "winner is X (1)"
    (is (= (final-score board-full-grid)
           0))))

(deftest valid-int-string-positive-test
  (testing "valid-int-string positive"
    (is (= (valid-int-string "76")
           "76"))))

(deftest valid-int-string-negative-test
  (testing "valid-int-string negative"
    (is (= (valid-int-string "cat")
           nil))))

(deftest get-word-sequence-test
  (testing "get-word-sequence test"
    (is (= (get-word-sequence "cat dog 7 banana")
           '("cat" "dog" "7" "banana")))))

(deftest convert-seq-to-int-negative-test
  (testing "convert-seq-to-int negative"
    (is (= (convert-seq-to-int ["cat" "dog" "7" "banana"])
           nil))))

(deftest convert-seq-to-int-positive-test
  (testing "convert-seq-to-int positive"
    (is (= (convert-seq-to-int ["4" "5" "7" "6"])
           '(4 5 7 6)))))

(deftest convert-int-seq-to-0-idx-test
  (testing "convert-int-seq-to-0-idx"
    (is (= (convert-int-seq-to-0-idx [1 3])
           '(0 2)))))

