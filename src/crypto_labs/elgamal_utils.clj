(ns crypto-labs.elgamal-utils
    (:require [clojure.core.typed :as t]))

(t/ann ^:no-check generate-primes [ -> (t/ASeq t/Int)])
(defn generate-primes []
    (let [reinsert (fn [table x prime]
                       (update-in table [(+ prime x)] conj prime))]
        (defn primes-step [table d]
            (if-let [factors (get table d)]
                (do
                (t/print-env "primes")
                (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                       (inc d)))
                (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                               (inc d))))))
        (primes-step {} 2)))

(t/ann ^:no-check mod-pow [t/Int t/Int t/Int -> t/Int])
(defn mod-pow
  "a^b mod n"
  [a b n]
  (let [[ba bb bn] (map biginteger [a b n])]
      (-> ba
          (.modPow bb bn)
          .intValue)))

(t/ann ^:no-check mod-inverse [t/Int t/Int -> t/Int])
(defn mod-inverse [x n]
    (let [[bx bn] (map biginteger [x n])]
        (-> bx
            (.modInverse bn)
            .intValue)))

(t/ann mod-eq [t/Int t/Int t/Int -> t/Bool])
(defn mod-eq
  "a ≡ b (mod n)"
  [a b n]
  (= (mod (* (mod a n)
             (mod b n)) n)
     (mod (* a b) n)))

(t/ann between? [t/Int t/Int t/Int -> t/Bool])
(defn between? [from to x]
    (and (< x to) (>= x from)))

(t/ann gcd [t/Int t/Int -> t/Int])
(defn gcd [a b]
    (if (zero? b)
        a
        (recur b (mod a b))))

(t/ann random [t/Int t/Int -> t/Int])
(defn random [from to]
    {:pre [(> to from)]
     :post [(between? from to %)]}
    (+ (rand-int (- to from)) from))


(t/ann next-until (t/All [x] [[x -> t/Bool] [ -> x] -> x]))
(defn next-until
  "Generates new value using `gen` function until `pred` is true"
  [pred gen]
  (t/loop [value :- x (gen)]
      (if (pred value)
          value
          (recur (gen)))))

(t/ann random-prime-number [t/Int t/Int -> t/Int])
(defn random-prime-number [from to]
    {:pre [(> to from)]
     :post [(between? from to %)]}
    (->> (generate-primes)
         (drop-while (t/fn [x :- t/Int] (< x from)))
         (take-while (t/fn [x :- t/Int] (<= x to)))
         rand-nth))

(t/ann generate-session-key [t/Int -> t/Int])
(defn generate-session-key [p]
    (next-until (t/fn [x :- t/Int]
                    (= (gcd x (dec p)) 1))
                #(random 2 (dec p))))
