(ns database
  (:require [ysera.test :refer [is= is]]))
(def state (atom {}))

(defn create-deck []
  (into [] (for [s [100 200 300 400]
                 n [2 3 4 5 6 7 8 9 10 11 12 13 14]]
             (+ s n))))

(defn init-state! [deck hand no-of-hands no-of-discards score-to-beat]
  (swap! state assoc :deck deck)
  (swap! state assoc :hand hand)
  (swap! state assoc :discards no-of-discards)
  (swap! state assoc :hands no-of-hands)
  (swap! state assoc :score 0)
  (swap! state assoc :score-to-beat score-to-beat)
  ;;Settings
  (swap! state assoc :show_menu true)
  )

(defn create-initial-state! []
  (init-state! (shuffle (create-deck)) [] 5 3 600)
  )

(defn new-deck!
  ;puts a new deck in state discarding any remaining cards in the old deck
  []
  (swap! state assoc :deck (shuffle (create-deck))))

(defn decrease-discards!
  []
  (assert (< 0 (:discards @state)))
  (swap! state assoc :discards (dec (:discards @state))))

(defn decrease-hands!
  []
  (assert (< 0 (:hands @state)))
  (swap! state assoc :hands (dec (:hands @state))))

(defn blind-defeated? []
  (<= (:score-to-beat @state) (:score @state)))

(defn hands-left? []
  (< 0 (:hands @state)))
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

