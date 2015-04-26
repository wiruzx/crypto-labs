(ns crypto-labs.playfair)

(def side 5)
(def default-x-char \x)
(def alphabet #{\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \r \s \t \u \v \w \x \y \z})
(def default-key [\p \l \a \y \f
                  \i \r \e \x \m
                  \b \c \d \g \h
                  \j \k \n \o \s
                  \t \u \v \w \z])

(defn fix-message
    ([message]
     (fix-message message []))
    ([[a b & rest] acc]
     (if (seq rest)
         (if (= a b)
             (recur (cons b rest) (conj acc a default-x-char))
             (recur rest (conj acc a b)))
         (conj acc a (or b default-x-char)))))

(defn digraphs [message]
    (->> message
         fix-message
         (partition-all 2)
         (mapv vec)))

(defn normalize [message]
    (->> message
         clojure.string/lower-case
         (filter alphabet)
         (apply str)))

(defn generate-key [keyword]
    {:pre [(>= (count keyword) 6)]}
    (let [distinct-keyword (-> keyword
                               normalize
                               distinct)
          filtered-alphabet (-> distinct-keyword
                                set
                                complement
                                (filter alphabet))]
        (partition side (concat distinct-keyword filtered-alphabet))))

(defn codec-index [shiftf [[xa ya] [xb yb]]]
    (cond
        (= xa xb) [[xa (shiftf ya)] [xb (shiftf yb)]]
        (= ya yb) [[(shiftf xa) ya] [(shiftf xb) yb]]
        :else [[xb ya] [xa yb]]))

(defn find2D [matrix item]
    (for [[y row] (map-indexed vector matrix)
          [x val] (map-indexed vector row)
          :when (= val item)]
        [x y]))

(defn get2D [matrix [x y]]
    (-> matrix
        (nth y)
        (nth x)))

(defn codec-digraph [shiftf key chars]
    (->> chars
         (mapv (partial find2D key))
         (mapv first)
         (codec-index shiftf)
         (mapv (partial get2D key))))

(defn shift-up [x]
    (if (< x (dec side))
        (inc x)
        0))

(defn shift-down [x]
    (if (= x 0)
        (dec side)
        (dec x)))

(defn encrypt [key message]
    (->> message
         normalize
         digraphs
         (mapv (partial codec-digraph shift-up key))
         (reduce (partial apply str) "")))

(defn decrypt [key message]
    (->> message
         normalize
         digraphs
         (map (partial codec-digraph shift-down key))
         flatten
         (remove #{default-x-char})
         (apply str)))
