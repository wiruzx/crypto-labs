(ns crypto-labs.elgamal
    (:refer-clojure :exclude [ensure])
    (:require [crypto-labs.elgamal-utils :as u]))

;; Records

(defrecord PublicKey [p g y])

(defrecord Keys [public private])

(defrecord Signature [r s])

;; Public API

(defn generate-keys []
    (let [p (u/random-prime-number 2000 10000)
          g (u/find-primitive-root p)
          x (u/random 2 p)
          y (u/mod-pow g x p)]
        (Keys. (PublicKey. p g y) x)))

(defn sign [{:keys [public private]} m]
    (let [{:keys [p g y]} public
          x private
          k (u/generate-session-key p)
          r (u/mod-pow g k p)
          s (mod (* (u/mod-inverse k (dec p))
                    (- m (* x r))) (dec p))] ;; (m - x * r) * k ^ -1  (mod p - 1)
        (Signature. r s)))

(defn ensure [public sign message]
    (let [{:keys [p g y]} public
          {:keys [r s]} sign]
        (and (< 0 r p)
             (< 0 s (- p 1))
             (= (mod (* (u/mod-pow y r p)
                        (u/mod-pow r s p)) p)
                (u/mod-pow g message p)))))
