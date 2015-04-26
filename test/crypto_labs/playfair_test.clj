(ns crypto-labs.playfair-test
    (:require [clojure.test :refer :all]
              [crypto-labs.playfair :refer :all]))

(deftest fix-message-test
    (let [fix-message-str (comp (partial apply str) fix-message)]

        ;; even
        (testing "with even number of chars"
            (is (= (fix-message-str "abcdef") "abcdef")))
        (testing "with even number of chars and repeated chars"
            (is (= (fix-message-str "abbcde") "abbcde")))
        (testing "with even number of chars and separated repeated chars"
            (is (= (fix-message-str "abccde") "abcxcdex")))

        ;; odd
        (testing "with odd number of chars"
            (is (= (fix-message-str "abcdefg") "abcdefgx")))
        (testing "with odd number of chars and repeated chars"
            (is (= (fix-message-str "abbcdef") "abbcdefx")))
        (testing "with odd number of chars and separated repeated chars"
            (is (= (fix-message-str "abccdef") "abcxcdef")))))

(deftest create-digraphs-tests
    (is (= (digraphs "abcdef")
           [[\a \b] [\c \d] [\e \f]])))

(deftest generating-key-tests
    (is (= (generate-key "playfair example")
           [[\p \l \a \y \f]
            [\i \r \e \x \m]
            [\b \c \d \g \h]
            [\j \k \n \o \s]
            [\t \u \v \w \z]])))

(deftest codec-index-tests
    (let [f (partial codec-index shift-up)]
        (testing "In one column"
            (is (= (f [[3 1] [3 3]])
                   [[3 2] [3 4]])))
        (testing "In one column, last item"
            (is (= (f [[2 1] [2 4]])
                   [[2 2] [2 0]])))
        (testing "In one row"
            (is (= (f [[1 2] [3 2]])
                   [[2 2] [4 2]])))
        (testing "In one row, last item"
            (is (= (f [[2 2] [4 2]])
                   [[3 2] [0 2]])))
        (testing "Different rows and columns"
            (is (= (f [[1 1] [3 4]])
                   [[3 1] [1 4]])))))


(let [matrix [[1 2 3]
              [4 5 6]
              [7 8 9]]]

    (deftest find2D-tests
        (testing "Finding 3"
            (is (= (first (find2D matrix 3)) [2 0])))
        (testing "Finding 5"
            (is (= (first (find2D matrix 5)) [1 1]))))

    (deftest get2D-tests
        (testing "Getting [0 0]"
            (is (= (get2D matrix [0 0]) 1)))
        (testing "Getting [1 2]"
            (is (= (get2D matrix [1 2]) 8)))))


(let [key [[\p \l \a \y \f]
           [\i \r \e \x \m]
           [\b \c \d \g \h]
           [\j \k \n \o \s]
           [\t \u \v \w \z]]]
      
      (deftest codec-digraph-tests
          (let [f (partial codec-digraph shift-up key)]
              (testing "In different cols and rows"
                  (is (= (f [\e \o]) [\x \n])))
              (testing "In the same column"
                  (is (= (f [\x \o]) [\g \w])))
              (testing "In the same column, last item"
                  (is (= (f [\l \u]) [\r \l])))
              (testing "In the same row"
                  (is (= (f [\k \o]) [\n \s])))
              (testing "In the same row, last item"
                  (is (= (f [\e \m]) [\x \i])))))
      
      (deftest encrypt-tests
          (is (= (encrypt key "Hide the gold in the tree stump")
                 "bmndzbxdkybejvdmuixmmnuvif")))
      
      (deftest decrypt-tests
          (is (= (decrypt key "bmndzbxdkybejvdmuixmmnuvif")
                 "hidethegoldinthetreestump"))))

