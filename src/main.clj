(ns main
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer [pprint]])
  (:require [ysera.test :refer [is=]])
  (:require [clojure.math.combinatorics :as combo])
  (:require [database :refer [state create-initial-state! new-deck! get-cards-as-strings]])
  (:require [scoring :refer [get-score scores]])
  )

(defonce hand-size 8)

(defn important
  {:test (fn []
           (is= (important "Anton") "Älskar dig Anton ❤")
           (is= (important "Pappa") "Älskar dig Pappa ❤")
           )}
  [name]
  (str
    (->> [196 108 115 107 97 114 32 100 105 103 32]
         (reduce (fn [ack val]
                   (str ack (char val)))
                 ""))
    name (char 32) \u2764))

(defn toggle-menu! [state]
  (swap! state assoc :show_menu (if (:show_menu @state)
                                  false
                                  true)))

(declare discard!)
(defn add-to-score!
  ;Adds value to score
  [state value]
  (swap! state assoc :score (+ value (:score @state))))


(defn deal!
  ;deals cards to the player so the player have hand-size cards
  ;if the deck does not have enough cards it is refilled
  [state]
  (let [missing (- hand-size (count (:hand @state)))
        enough? (<= missing (count (:deck @state)))]
    (println "deal! missing" missing "enough?" enough?)
    (if (not enough?)
      (do
        (swap! state assoc :hand (into [] (sort (concat (:hand @state) (:deck @state)))))
        (new-deck!)
        (deal! state))
      (do
        (swap! state assoc :hand (into [] (sort (concat (:hand @state) (take missing (:deck @state))))))
        (swap! state assoc :deck (subvec (:deck @state) missing)))
      )))

(defn play-indices! [state card-ixs]
  (let [chosen-cards (->> (:hand @state)
                          (map-indexed vector)
                          (filter (fn [[k _]]
                                    (some #(= (+ k 1) %) card-ixs)))
                          (reduce (fn [a v]
                                    (conj a (second v)))
                                  []))]
    (println "You played: " (get-cards-as-strings chosen-cards) "for" (get-score chosen-cards) "pts")
    (add-to-score! state (get-score chosen-cards))
    (discard! state chosen-cards)))

(defn play-cards! [state cards]
  (println "You played: " (get-cards-as-strings cards) "for" (get-score cards) "pts")
  (add-to-score! state (get-score cards))
  (discard! state cards))

(defn get-card-ixs-from-user
  ;Returns a vector with the cards the user wants to do "action" with
  [action]
  (loop [input []]
    (if (= 5 (count (set input)))                           ;;input must be five distinct cards
      input
      (do
        (println "Enter five cards (ex. 1 3 5 6 8)")
        (recur (reduce (fn [a v]
                         (conj a (Integer/parseInt v)))
                       []
                       (str/split (read-line) #"\s+")))))))
(defn get-cards-from-ixs
  ;returns the cards from the hand that corresponds to the given indexes
  ;typically used in conjunction with get-card-ixs-from-user
  {:test (fn []
           (is= (get-cards-from-ixs ["A" "B" "C"] [1 3]) ["A" "C"])
           (is= (get-cards-from-ixs ["A" "B" "C"] [2]) ["B"]))}
  [hand ixs]
  (->> hand
       (map-indexed vector)
       (filter (fn [[k _]]
                 (some #(= (+ k 1) %) ixs)))
       (reduce (fn [a v]
                 (conj a (second v)))
               [])))

(defn discard
  ;;discards the cards in cards-to-discard from hand
  {:test (fn []
           (is= (discard [101 102 103 104 105] [101 103 105]) [102 104])
           (is= (discard [101 102 103 104 105] [101 103 105 106]) [102 104])
           (is= (discard [101 102 103] []) [101 102 103])
           (is= (discard [] [101 102 103]) []))}
  [hand cards-to-discard]
  (println "cards-to-discard" cards-to-discard)
  (->> hand
       (remove (fn [v]
                 (some #(= v %) cards-to-discard)))
       (into [])))

(defn discard! [state cards-to-discard]
  (swap! state assoc :hand (discard (:hand @state) cards-to-discard)))

(defn print-hand
  ;prints hand in tabular format with the number/index under each card
  [hand]
  (let [hand-as-strings (get-cards-as-strings hand)
        indices (range 1 (+ 1 (count hand-as-strings)))]
    (println (apply str (interleave (repeat "\t") hand-as-strings)))
    (println (apply str (interleave (repeat "\t") indices)))
    ))

(defn auto-play
  ;Finds the optimal play given a hand and the current scoring table
  [hand]
  (->> (combo/combinations (set hand) 5)
       (reduce (fn [ack val]
                 (into ack {(get-score val) val})
                 )
               (sorted-map))
       (last)
       (val)))

(defn -main [& args]
  (println (repeat 3 "\u2660") " Let's play Balatro!" (repeat 3 "\u2660"))
  (create-initial-state!)
  (deal! state)
  (loop [round-count 1]
    (if (:show_menu @state)
      (println "Menu\n\tq - quit\n\tp - play\n\td - discard\n\ts - score chart\n\tt - toggle menu"))

    (println "|------" "Round" round-count
             "---------" "Score" (:score @state)
             "---------" "Cards left" (count (:deck @state))
             "--------|")
    (print-hand (:hand @state))
    (print "Enter command ")
    (flush)
    (case (read-line)
      "q" nil
      "p" (do (play-indices! state (get-card-ixs-from-user "play"))
              (deal! state)
              (recur (inc round-count)))
      "." (do (play-cards! state (auto-play (:hand @state)))
              (deal! state)
              (recur (inc round-count)))
      "d" (do (discard! state (get-cards-from-ixs (:hand @state) (get-card-ixs-from-user "discard")))
              (deal! state)
              (recur round-count))
      "s" (do (pprint scores)
              (recur round-count))
      "t" (do (toggle-menu! state)
              (recur round-count))
      (do (println "Invalid command")
          (recur round-count))))
  (println "Sayonara. You scored " (:score @state)))

(comment
  (important "Anton")
  (-main)
  )



