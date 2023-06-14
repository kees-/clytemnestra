(ns kees.clytemnestra.main-test
  (:require [clojure.test :as test :refer [deftest testing is]]
            [kees.clytemnestra.impl :as impl]
            [kees.clytemnestra.main :as main]
            [kees.clytemnestra.piet :as lang]))

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

(deftest piet-commands
  (testing "Testing push and pop"
    (is (= '(5 6 7) (:stack (lang/ppush {:stack '(6 7)
                                         :value 5})))
        "Push works")
    (is (= '(6 7) (:stack (lang/ppop {:stack '(5 6 7)})))
        "Pop works"))

  (testing "Testing arithmetic"
    (is (= '(7 5 6) (:stack (lang/p+ {:stack '(3 4 5 6)})))
        "Addition works")

    (is (= '(1 5 6) (:stack (lang/p- {:stack '(3 4 5 6)})))
        "Positive result subtraction works")
    (is (= '(-1 5 6) (:stack (lang/p- {:stack '(4 3 5 6)})))
        "Positive result subtraction works")

    (is (= '(2 20) (:stack (lang/p÷ {:stack '(5 10 20)})))
        "Division is good")
    (is (= '(3 20) (:stack (lang/p÷ {:stack '(3 10 20)})))
        "Division is good")
    (is (= '(0 10 20) (:stack (lang/p÷ {:stack '(0 10 20)})))
        "Zero division ignored")

    (is (= '(1 9) (:stack (lang/p% {:stack '(3 10 9)})))
        "Mod is good")
    (is (= '(0 10 20) (:stack (lang/p% {:stack '(0 10 20)})))
        "Zero mod ignored"))

  (testing "Testing misc operations"
    (is (= '(1 5) (:stack (lang/pnot {:stack '(0 5)})))
        "0 flips to 1")
    (is (= '(0 5) (:stack (lang/pnot {:stack '(1 5)})))
        "1 flips to 0")
    (is (= '(0 5) (:stack (lang/pnot {:stack '(5 5)})))
        "Nonzero flips to 0")

    (is (= '(1 9) (:stack (lang/p> {:stack '(3 6 9)})))
        "6 > 3")
    (is (= '(0 9) (:stack (lang/p> {:stack '(6 3 9)})))
        "3 >/ 6")
    (is (= '(0 9) (:stack (lang/p> {:stack '(3 3 9)})))
        "3 >/ 3")

    (is (= '(9 9) (:stack (lang/pdupe {:stack '(9)})))
        "Dupe works")
    (is (= '(9 9 6 3) (:stack (lang/pdupe {:stack '(9 6 3)})))
        "Dupe with deeper stack works"))

  (testing "Testing pointer madness"
    (is (= {:stack '(4) :compass [:right -1]}
           (lang/pdp {:stack '(0 4) :compass [:right -1]}))
        "DP non-rotation works")
    (is (= {:stack '(4) :compass [:left -1]}
           (lang/pdp {:stack '(2 4) :compass [:right -1]}))
        "Clockwise DP rotation works")
    (is (= {:stack '(4) :compass [:down -1]}
           (lang/pdp {:stack '(-3 4) :compass [:right -1]}))
        "Clockwise DP rotation works")
    (is (= {:stack '(4) :compass [:up -1]}
           (lang/pdp {:stack '(27 4) :compass [:right -1]}))
        "Very large value DP rotation works")

    (is (= {:stack '(4) :compass [:right 1]}
           (lang/pdp {:stack '(0 4) :compass [:right 1]}))
        "DP non-rotation works")
    (is (= {:stack '(4) :compass [:right 1]}
           (lang/pcc {:stack '(1 4) :compass [:right -1]}))
        "Clockwise CC rotation works")
    (is (= {:stack '(4) :compass [:right -1]}
           (lang/pcc {:stack '(-2 4) :compass [:right -1]}))
        "Clockwise CC rotation works")
    (is (= {:stack '(4) :compass [:right 1]}
           (lang/pcc {:stack '(33 4) :compass [:right -1]}))
        "Very large value CC rotation works"))

  (testing "Testing roll"
    (is (= '(3 4 5 1 2 6 7) (:stack (lang/proll {:stack '(5 2 1 2 3 4 5 6 7)})))
        "Good roll is good")
    (is (= '(1 2 3 4 5) (:stack (lang/proll {:stack '(3 3 1 2 3 4 5)})))
        "depth=n roll is ignored")
    (is (= '(10 4 1 2 3 4 5) (:stack (lang/proll {:stack '(10 4 1 2 3 4 5)})))
        "Roll out of its depth is ignored")
    (is (= '(-2 3 1 2 3 4 5) (:stack (lang/proll {:stack '(-2 3 1 2 3 4 5)})))
        "Negative depth roll is ignored")
    (is (= '(4 5 1 2 3 6 7) (:stack (lang/proll {:stack '(5 8 1 2 3 4 5 6 7)})))
        "Weird roll is weird but okay"))
  
  ;; Input can't really be automatically tested

  (testing "Testing output"
    (is (= {:stack '(33 11) :output '(55 \i \h)}
           (lang/poutn {:stack '(55 33 11) :output '(\i \h)}))
        "Testing integer output")
    (is (= {:stack '(55 77) :output '(\! \) \: \i \h)}
           (lang/poutc {:stack '(33 55 77) :output '(\) \: \i \h)}))
        "Testing character output")
    (is (= {:stack '(20 30) :output '(10)}
           (lang/poutc {:stack '(20 30) :output '(10)}))
        "Out of range output is skipped"))

  (testing "Testing empty/underloaded conditions"
    (let [blank {:stack '()}]
      (is (= '() (:stack (lang/ppop blank)))
          "Popping empty list returns empty list")

      (is (= '() (:stack (lang/p- blank)))
          "Underloaded arithmetic (0) ignored")
      (is (= '(10) (:stack (lang/p- {:stack '(10)})))
          "Underloaded arithmetic (1) ignored")

      (is (= '() (:stack (lang/pnot blank)))
          "Underloaded not doesn't push ghost 0")
      (is (= '() (:stack (lang/p> blank)))
          "Underloaded greater than doesn't push ghost value")
      (is (= '() (:stack (lang/pdupe blank)))
          "Empty dupe does nothing")

      (is (= {:stack '() :compass [:right -1]}
             (lang/pdp {:stack '() :compass [:right -1]}))
          "Empty stack DP rotation doesn't damage state")
      (is (= {:stack '() :compass [:right -1]}
             (lang/pcc {:stack '() :compass [:right -1]}))
          "Empty stack CC flipping doesn't damage state")

      (is (= '() (:stack (lang/proll {:stack '()})))
          "Empty roll is ignored")

      (is (= {:stack '() :output '(\h \o)}
             (lang/poutn {:stack '() :output '(\h \o)}))
          "Testing integer output on empty stack")
      (is (= {:stack '() :output '(\w \o \w)}
             (lang/poutc {:stack '() :output '(\w \o \w)}))
          "Testing character output on empty stack"))))
