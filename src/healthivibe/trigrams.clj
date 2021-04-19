(ns healthivibe.trigrams
  (:require [clojure.string :as str])
  (:gen-class))

(def punctuation #{\! \? \.})

(def book (slurp "resources/book.txt"))

(defn drop-split-sentences [trigram]
  (remove (fn [[a b]]
            (or (punctuation (last a))
                (punctuation (last b))))
          trigram))

(defn gather-trigrams
  [text & [backwards]]
  (->> (str/split text #"[^\w-[’\'.!?\-]]")
       (filter seq)
       (#(if backwards (reverse %) %))
       (partition 3 1)
       drop-split-sentences))

(defn group-trigrams
  "Takes a sequence of trigrams, a trigram is a three element vector, e.g. [\"a\" \"b\" \"c\"].
   Returns a trigram map, the keys being a two element vector of common starts to the trigram,
   the values being a vector of any existing third word that is followed after the two.
   Based on clojure.core/group-by"
  [trigrams]
  (persistent!
   (reduce
    (fn [ret x]
      (let [k [(first x) (second x)]
            v (last x)]
        (assoc! ret k (conj (get ret k []) v))))
    (transient {}) trigrams)))

(defn generate-trigram-map
  [text & [backwards]]
  (group-trigrams (gather-trigrams text backwards)))


(defn find-key-starting-with [trigram-map x]
  (-> (filter (fn [[a b]]
                (= a x))
              (keys trigram-map))
      rand-nth))

(defn find-key-ending-with [trigram-map x]
  (-> (filter (fn [[a b]]
                (= b x))
              (keys trigram-map))
      seq
      rand-nth))

(defn generate-sentence [trigram-map n & [start-key]]
  (loop [i (- n 2)
         k (or start-key (rand-nth (keys trigram-map)))
         s (str/capitalize (apply str (interpose " " k)))]
    (if (zero? i)
      (str s ".")
      (let [; sometimes the bigram only exists at sentence end, but important to get our full quota of words, so try some different approaches to get an appropriate word...
            values (or (seq (remove (fn [s] (punctuation (last s)))
                                    (get trigram-map k)))
                       (get trigram-map k)
                       (seq (remove (fn [s] (punctuation (last s)))
                                    (get trigram-map (find-key-ending-with trigram-map (re-find #"[\w\-\'’]+" (second k))))))
                       (rand-nth (vals trigram-map)))
  
            v (rand-nth values)
            v (re-find #"[\w\-\'’]+" v)]
        (recur (dec i) [(last k) v] (str s " " v))))))

(defn generate-sentence-backwards [trigram-map n start-key]
  (loop [i n
         k start-key
         s (apply str (reverse (interpose " " k)))]
    (if (zero? i)
      (str/capitalize s)
      ;; problem with the above is it lowercases words like "I", could try something like the following but that has the problem of persisting capitalized beginning of sentence words
      ;; (str (str/capitalize (subs s 0 1))
      ;;      (subs s 1 (count s)))
      (let [values (or (get trigram-map k) ; sometimes the bigram only exists at sentence end, but important to get our full quota of words, so pick another random word to continue in this case.
                       (get trigram-map (find-key-ending-with trigram-map (re-find #"[\w\-\'’]+" (second k))))
                       (rand-nth (vals trigram-map)))
            v (re-find #"[\w\-\'’]+" (rand-nth values))]
        (recur (dec i) [(last k) v] (str v " " s))))))

(defn generate-sentences [trigram-map n]
  (->> (repeatedly n (partial generate-sentence trigram-map (+ 12 (rand-int 20))))
       (interpose " ")
       (apply str)))

(defn generate-sentence-containing [trigram-map
                                    trigram-map-backwards
                                    word]
  (let [beginning (generate-sentence-backwards trigram-map-backwards
                                               (+ 4 (rand-int 12))
                                               (find-key-starting-with trigram-map-backwards word))
        last-two-words (->> (str/split beginning #" ")
                            (take-last 2)
                            vec)]
    (str
     beginning
     " "
     (subs (generate-sentence trigram-map
                              (+ 3 (rand-int 12))
                              last-two-words)
           (+ 2 (count (apply str last-two-words)))))))

(def trigram-map (generate-trigram-map book))

(def trigram-map-backwards (generate-trigram-map book true))



(defn -main
  [& args]
  (println "Task 1: generate a sequence of 50 words")
  (println (generate-sentence trigram-map 50))
  (println "\n\nTask 2: Generate 10 full sentences")
  (println (generate-sentences trigram-map 10))
  (println "\n\nTask 2: Given a word from the text, generate a sentence which contains it anywhere inside it (using example of 'great')")
  (println (generate-sentence-containing trigram-map trigram-map-backwards "great")))

