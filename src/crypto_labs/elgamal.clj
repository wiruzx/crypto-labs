(ns crypto-labs.elgamal
    (:refer-clojure :exclude [ensure])
    (:require [clojure.core.typed :as t]
              [crypto-labs.elgamal-utils :as u]))

;; Aliases

(t/defalias PrivateKey t/Int)

;; Records

(t/ann-record PublicKey [p :- t/Int, g :- t/Int, y :- t/Int])
(defrecord PublicKey [p g y])

(t/ann-record Keys [public :- PublicKey, private :- PrivateKey])
(defrecord Keys [public private])

(t/ann-record Signature [r :- t/Int, s :- t/Int])
(defrecord Signature [r s])

;; Public API

(t/ann generate-keys [ -> Keys])
(defn generate-keys []
    (let [p (u/random-prime-number 2000 10000)
          g (u/find-primitive-root p)
          x (u/random 2 p)
          y (u/mod-pow g x p)]
        (Keys. (PublicKey. p g y) x)))

(t/ann sign [Keys t/Int -> Signature])
(defn sign [{:keys [public private]} m]
    (let [{:keys [p g y]} (t/ann-form public PublicKey)
          x private
          k (u/generate-session-key p)
          r (u/mod-pow g k p)
          s (mod (* (u/mod-inverse k (dec p))
                    (- m (* x r))) (dec p))] ;; (m - x * r) * k ^ -1  (mod p - 1)
        (Signature. r s)))

(t/ann ensure [PublicKey Signature t/Int -> t/Bool])
(defn ensure [pub-key sign message]
    false)
