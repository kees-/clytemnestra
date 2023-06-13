(ns kees.clytemnestra.main-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [kees.clytemnestra.impl :as impl]
            [kees.clytemnestra.main :as main]))

(deftest interpreter-accuracy
  (testing "Testing pointer traversal correctness"
    (let [left-state {:pointer [8 3] :compass [:down -1]
                      :block #{[7 3] [7 4] [7 5]
                               [8 3] [8 4] [8 5] [8 6]
                               [9 3] [9 4] [9 5]}}
          right-state (update-in left-state [:compass 1] -)]
      (is (= [8 6] (impl/find-corner left-state))
          "A downwards DP should find the absolute lowest edge")
      (is (= [8 6] (impl/find-corner right-state))
          "A downwards DP should find the absolute lowest edge"))

    (let [state {:pointer [8 3] :compass [:right -1]
                 :block #{[7 3] [7 4] [7 5]
                          [8 3] [8 4] [8 5] [8 6]
                          [9 3] [9 4] [9 5]
                          [10 5]}}]
      (is (= [10 5] (impl/find-corner state))
          "A rightwards DP should find the absolute rightmost edge"))

    (let [corner (main/piet {:file "programs/corner-test.png"
                             :limit 20 :verbose? false})]
      (is (= [2 11] (:pointer corner))
          "Pointer should find the correct output in a jagged codel block"))

    (let [pi (main/piet {:file "programs/pi.png"
                         :codel-size 3 :limit 30 :verbose? false})]
      (is (= [131 58] (:pointer pi))
          "Pointer should get through very large and irregular blocks (like pi circle)"))

    (let [hw2 (main/piet {:file "programs/hello-world-2.png"
                          :limit 40 :verbose? false})]
      (is (= [80 3] (:pointer hw2))
          "Determine pointer correctness for hello-world-2"))))
