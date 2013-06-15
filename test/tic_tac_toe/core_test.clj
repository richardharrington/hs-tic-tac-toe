(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(def test-grid 
    [ ["*" "*" "X"] 
      ["O" "*" "*"]
      ["*" "*" "*"] ] )

(deftest replace-item-test
  (testing "replacing an item in a vector"
    (is (= (replace-item [1 2 3 4 5 6 7 8 9] 4 "apples")
           [1 2 3 4 "apples" 6 7 8 9]))))

(deftest replace-item-nested-2d-test
  (testing "replacing an item in a 2d vector"
    (is (= (replace-item-nested-2d [[1 2] [3 4] [5 6]] 1 0 "apples")
           [[1 2] ["apples" 4] [5 6]]))))

(deftest marker-at-test-1
  (testing "marker at a full square"
    (is (= (marker-at test-grid 2 0) "X"))))

(deftest marker-at-test-2
  (testing "marker at an empty square"
    (is (= (marker-at test-grid 2 2) "*"))))

(deftest add-marker-test
  (testing "adding a marker to a grid"
    (is (= (add-marker test-grid "X" 1 1) 
           [ ["*" "*" "X"] 
             ["O" "X" "*"]
             ["*" "*" "*"] ] ))))




