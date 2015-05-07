(ns crypto-labs.elgamal-utils)

(defn generate-primes []
    (let [reinsert (fn [table x prime]
                       (update-in table [(+ prime x)] conj prime))]
        (defn primes-step [table d]
            (if-let [factors (get table d)]
                (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                       (inc d))
                (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                               (inc d))))))
        (primes-step {} 2)))

(defn mod-pow
  "a^b mod n"
  [a b n]
  (let [[ba bb bn] (map biginteger [a b n])]
      (-> ba
          (.modPow bb bn)
          .intValue)))

(defn mod-inverse [x n]
    (let [[bx bn] (map biginteger [x n])]
        (-> bx
            (.modInverse bn)
            .intValue)))

(defn- between? [from to x]
    (and (< x to) (>= x from)))

(defn gcd [a b]
    (if (zero? b)
        a
        (recur b (mod a b))))

(defn random [from to]
    {:pre [(> to from)]
     :post [(between? from to %)]}
    (+ (rand-int (- to from)) from))

(defn next-until
  "Generates new value using `gen` function until `pred` is true"
  [pred gen]
  (loop [value (gen)]
      (if (pred value)
          value
          (recur (gen)))))

(defn random-prime-number [from to]
    {:pre [(> to from)]
     :post [(between? from to %)]}
    (->> (generate-primes)
         (drop-while (t/fn [x :- t/Int] (< x from)))
         (take-while (t/fn [x :- t/Int] (< x to)))
         rand-nth))

(defn generate-session-key [p]
    (next-until (fn [x]
                    (= (gcd x (dec p)) 1))
                #(random 2 (dec p))))

(defn find-primitive-root [p]
    (next-until (fn [g]
                    (and (not= (mod-pow g 2 p) 1)
                         (not= (mod-pow g (/ (- p 1) 2) p) 1)))
                #(random 2 p)))
