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

(deftest replace-item-test
  (testing "replacing an item in a vector"
    (is (= (replace-item [1 2 3 4 5 6 7 8 9] 4 "apples")
           [1 2 3 4 "apples" 6 7 8 9]))))

(deftest replace-item-nested-2d-test
  (testing "replacing an item in a 2d vector"
    (is (= (replace-item-nested-2d [[1 2] [3 4] [5 6]] "apples" 1 0)
           [[1 2] ["apples" 4] [5 6]]))))

(deftest coordinates-seq-test
  (testing "getting sequence of coordinates for 2d vector"
    (is (= (coordinates-seq [[9 9 8] [3 4 5]])
           [{:x 0 :y 0} {:x 0 :y 1} {:x 0 :y 2} 
            {:x 1 :y 0} {:x 1 :y 1} {:x 1 :y 2}]))))

(deftest marker-at-test-1
  (testing "marker at a full square"
    (is (= (marker-at test-grid {:x 2, :y 0}) "X"))))

(deftest marker-at-test-2
  (testing "marker at an empty square"
    (is (= (marker-at test-grid {:x 1, :y 2}) "*"))))

(deftest replace-marker-test
  (testing "adding a marker to a grid"
    (is (= (export-grid-for-display (replace-marker test-grid 
                                                    "X" 
                                                    {:x 1, :y 1}))
           [["X" "O" "X"] 
            ["O" "X" "O"]
            ["O" "*" "O"]]))))

(deftest free-squares-test
  (testing "finding free squares"
    (is (= (free-squares test-grid)
           [{:x 1 :y 1} {:x 1 :y 2}]))))




