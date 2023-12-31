(ns main
  (:require [clojure.string :as str])
  (:require [ysera.test :refer [is=]])
  (:require [clojure.math.combinatorics :as combo])
  (:require [database :refer [state create-initial-state! new-deck! get-cards-as-strings decrease-discards! decrease-hands! blind-defeated? hands-left?]])
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
        enough-cards? (<= missing (count (:deck @state)))]
    (if (not enough-cards?)
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
    (println "You played: " (get-cards-as-strings chosen-cards) "for" (get-score chosen-cards) "pts\n")
    (add-to-score! state (get-score chosen-cards))
    (discard! state chosen-cards)))

(defn play-cards! [state cards]
  (println "You played: " (get-cards-as-strings cards) "for" (get-score cards) "pts\n")
  (add-to-score! state (get-score cards))
  (discard! state cards))

(defn get-card-ixs-from-user
  ;Returns a vector with 1-5 cards that the user wants to do "action" with
  ;Duplicates are assumed to be accidental and are quietly removed before return.
  [action]
  (loop [input []]
    (if (and (< 0 (count input)) (> 6 (count input)))       ;;input must be at least one card
      (into [] (set input))                                 ;;remove duplicates
      (do
        (print "Enter the cards you want to" action "")
        (flush)
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
    (println (apply str (interleave (repeat "\t") indices)))))

(defn auto-play
  ;Plays (one of) the five card combinations that scores the highest, potentially zero points, in a given hand
  ;Assumes that the hand has at least five cards
  [hand]
  (->> (combo/combinations hand 5)
       (reduce (fn [ack val]
                 (into ack {(get-score val) val})
                 )
               (sorted-map))
       (last)
       (val)))

(defn pad-string-right [input-string desired-length]
  (str input-string (apply str (repeat (- desired-length (count input-string)) " "))))
(defn pad-string-left [input-string desired-length]
  (str (apply str (repeat (- desired-length (count input-string)) " ")) input-string))

(defn print-scorecard []
  (let [width 34]
    (println (apply str (repeat width "-")))
    (println (pad-string-left "Base" 22) (pad-string-left "Mult" 10))
    (println (apply str (repeat width "-")))
    (let [score-card (->> scores
                          (reduce (fn [ack [_ v]]
                                    (let [base-score (:base-score v)
                                          multiplier (:multiplier v)
                                          hand-type (:name v)
                                          sort-key (* base-score multiplier)]
                                      (conj ack (vector sort-key hand-type base-score multiplier)))) [])
                          (sort-by first >))
          max-name-length (reduce (fn [ack [_ val]]
                                    (max ack (count (:name val)))) 0 scores)]
      (doseq [[_ name base-score multiplier] score-card]
        (println (pad-string-right name max-name-length)
                 (pad-string-left (str base-score) 6)
                 "   x"
                 (pad-string-left (str multiplier) 5))))
    (println (apply str (repeat width "-")))
    (println)))

(defn -main [& args]
  (println (repeat 3 "\u2660") " Let's play Balatro!" (repeat 3 "\u2660"))
  (create-initial-state!)
  (deal! state)
  (loop [round-count 1]
    (if (and (not (blind-defeated?)) (hands-left?))
      (do
        (if (:show_menu @state)
          (println "Menu\n\tq - quit\n\tp - play\n\td - discard\n\ts - score chart\n\tt - toggle menu\n"))
        (println "|- Round" round-count
                 "-- Score" (:score @state)
                 "-- Blind" (:score-to-beat @state)
                 "-- Hands" (:hands @state)
                 "-- Discards" (:discards @state)
                 "---|")
        (print-hand (:hand @state))
        (print "\nEnter command ")
        (flush)
        (case (read-line)
          "q" nil
          "p" (do (if (< 0 (:hands @state))
                    (do
                      (decrease-hands!)
                      (play-indices! state (get-card-ixs-from-user "play"))
                      (deal! state)))
                  (recur (inc round-count)))
          "." (do (if (< 0 (:hands @state))
                    (do
                      (play-cards! state (auto-play (:hand @state)))
                      (decrease-hands!)
                      (deal! state)))
                  (recur (inc round-count)))
          "d" (do (if (< 0 (:discards @state))
                    (do
                      (decrease-discards!)
                      (discard! state (get-cards-from-ixs (:hand @state) (get-card-ixs-from-user "discard")))
                      (deal! state))
                    (println "\nNo discards left\n"))
                  (recur round-count))
          "s" (do (print-scorecard)
                  (recur round-count))
          "t" (do (toggle-menu! state)
                  (recur round-count))
          (do (println "Invalid command")
              (recur round-count))))))
  (if (blind-defeated?)
    (printf "Sayonara. You scored $%d and won the game!" (:score @state))
    (printf "Sayonara. You scored $%d but did not defeat the blind at %d :-(" (:score @state) (:score-to-beat @state))))


(comment
  (important "Anton")
  (-main)
  )


