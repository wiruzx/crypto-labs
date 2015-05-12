(ns crypto-labs.three-way)

;; Conts

(def STRT-E (int 0x0b0b))
(def STRT-D (int 0xb1b1))
(def NMBR 11)

;; Utility functions

(defn swap [v n f]
    (assoc v n (f (nth v n))))

;; Private functions

(defn rndcon-gen [start]
    (loop [rtab []
           current-start start
           i (int 0)]
        (if (<= i NMBR)
            (recur (conj rtab current-start)
                   (let [new-start (bit-shift-left current-start 1)]
                       (if (not= (bit-and new-start (int 0x10000)) 0)
                           (bit-xor new-start (int 0x11011))
                           new-start))
                   (inc i))
            rtab)))

(defn mu [m]
    (loop [b [0 0 0]
           a m
           i 0]
        (if (< i 32)
            (let [[b00 b01 b02] b
                  [a00 a01 a02] a
                  b10 (bit-shift-left b00 1)
                  b11 (bit-shift-left b01 1)
                  b12 (bit-shift-left b02 1)
                  b20 (if (not= (bit-and a02 1) 0) (bit-or b10 1) b10)
                  b21 (if (not= (bit-and a01 1) 0) (bit-or b11 1) b11)
                  b22 (if (not= (bit-and a00 1) 0) (bit-or b12 1) b12)
                  a10 (bit-shift-right a00 1)
                  a11 (bit-shift-right a01 1)
                  a12 (bit-shift-right a02 1)]
                (recur [b20 b21 b22] [a10 a11 a12] (inc i)))
            b)))

(defn gamma [a]
    (letfn [(f [n1 n2 n3]
              (bit-xor (nth a n1)
                       (bit-or (nth a n2) (bit-not (nth a n3)))))]
        [(f 0 1 2)
         (f 1 2 0)
         (f 2 0 1)]))

(defn thetta [a]
    (let [f (fn [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]
                (bit-xor a0
                         (bit-shift-right a1 16)
                         (bit-shift-left a2 16)
                         (bit-shift-right a3 16)
                         (bit-shift-left a4 16)
                         (bit-shift-right a5 24)
                         (bit-shift-left a6 8)
                         (bit-shift-right a7 8)
                         (bit-shift-left a8 24)
                         (bit-shift-right a9 16)
                         (bit-shift-left a10 16)
                         (bit-shift-right a11 24)
                         (bit-shift-left a12 8)))
          [a0 a1 a2] a]
        [(f a0 a0 a1 a1 a2 a1 a2 a2 a0 a2 a0 a2 a0)
         (f a1 a1 a2 a2 a0 a2 a0 a0 a1 a0 a1 a0 a1)
         (f a2 a2 a0 a0 a1 a0 a1 a1 a2 a1 a2 a1 a2)]))

(defn pi-1 [a]
    [(bit-xor (bit-shift-right (nth a 0) 10)
              (bit-shift-left (nth a 0) 22))
     (nth a 1)
     (bit-xor (bit-shift-left (nth a 2) 1)
              (bit-shift-right (nth a 2) 31))])


(defn pi-2 [a]
    [(bit-xor (bit-shift-left (unchecked-int (nth a 0)) 1)
              (bit-shift-right (unchecked-int (nth a 0)) 31))
     (nth a 1)
     (bit-xor (bit-shift-right (unchecked-int (nth a 2)) 10)
              (bit-shift-left (unchecked-int (nth a 2)) 22))])

(def rho (comp pi-2 gamma pi-1 thetta))

(defn xor [a k rcon]
    (-> a
       (swap 0 #(bit-xor % (bit-xor (nth k 0) (bit-shift-left rcon 16))))
       (swap 1 #(bit-xor % (nth k 1)))
       (swap 2 #(bit-xor % (bit-xor (nth k 2) rcon)))))

(defn rounds [a k rcon]
    (loop [result a
           i 0]
        (if (< i NMBR)
            (recur (-> result
                      (xor k (nth rcon i))
                     rho)
                   (inc i))
            result)))

;; Public functions

(defn encrypt [key message]
    (let [rcon (rndcon-gen STRT-E)]
        (-> message
           (rounds key rcon)
           (xor key (nth rcon NMBR))
           thetta)))

(defn decrypt [key message]
    (let [ki (-> key thetta mu)
          rcon (rndcon-gen STRT-D)]
        (-> message
           mu
           (rounds ki rcon)
           (xor ki (nth rcon NMBR))
           thetta
           mu)))
