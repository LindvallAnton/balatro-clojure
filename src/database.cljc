(ns database
  (:require [ysera.test :refer [is= is]]))
(def state (atom {}))

(defn create-deck []
  (into [] (for [s [100 200 300 400]
                 n [2 3 4 5 6 7 8 9 10 11 12 13 14]]
             (+ s n))))

(defn init-state! [deck hand choices]
  (swap! state assoc :deck deck)
  (swap! state assoc :hand hand)
  (swap! state assoc :choices choices)
  (swap! state assoc :show_menu true)
  (swap! state assoc :score 0))

(defn create-initial-state! []
  (init-state! (shuffle (create-deck)) [] [])
  )

(defn new-deck!
  ;puts a new deck in state discarding any remaining cards in the old deck
  []
  (swap! state assoc :deck (shuffle (create-deck)))
  )

(defn get-suit-number
  ;Convenience function that given a card number returns the suit number
  {:test (fn []
           (is= (get-suit-number 101) 1)
           (is= (get-suit-number 213) 2))}
  [card]
  (quot card 100))

(defn get-card-rank
  ;Convenience function that given a card number returns the card rank
  {:test (fn []
           (is= (get-card-rank 107) 7)
           (is= (get-card-rank 213) 13))}
  [card]
  (- card (* 100 (get-suit-number card))))

(defn get-card-ranks
  ;Convenience function that given a collection of card values returns the cards values
  {:test (fn []
           (is= (get-card-ranks [107 208 309]) [7 8 9])
           (is= (get-card-ranks [213]) [13]))}
  [cards]
  (->> cards (reduce (fn [ack val]
                       (conj ack (get-card-rank val)))
                     [])))

(defn get-cards-as-strings
  ;Transforms numeric representation of cards to human-readable text
  {:test (fn []
           (is= (get-cards-as-strings [111 212 313 414]) ["SJ" "DQ" "CK" "HA"])
           )}
  [cards]
  (->> cards
       (reduce (fn [a v]
                 (conj a (str (case (get-suit-number v)
                                1 "S"
                                2 "D"
                                3 "C"
                                4 "H"
                                )
                              (case (get-card-rank v)
                                11 "J"
                                12 "Q"
                                13 "K"
                                14 "A"
                                (get-card-rank v)
                                ))))
               [])))

