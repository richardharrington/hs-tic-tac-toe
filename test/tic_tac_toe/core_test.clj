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

(deftest import-grid-template-test
  (testing "importing grid template"
    (is (= (import-grid-template test-grid-template)
           test-grid))))

(deftest export-grid-for-display-test
  (testing "exporting grid for display"
    (is (= (export-grid-for-display test-grid)
           test-grid-template))))

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

(deftest marker-at-test-1
  (testing "marker at a full square"
    (is (= (marker-at test-grid [2 0]) "X"))))

(deftest marker-at-test-2
  (testing "marker at an empty square"
    (is (= (marker-at test-grid [1 2]) "*"))))

(deftest replace-marker-test
  (testing "adding a marker to a grid"
    (is (= (export-grid-for-display (replace-marker test-grid 
                                                    [1 1]
                                                    "X"))
           [["X" "O" "X"] 
            ["O" "X" "O"]
            ["O" "*" "O"]]))))

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