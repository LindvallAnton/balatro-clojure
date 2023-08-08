(ns main
  (:require [clojure.string :as str])
  (:require [ysera.test :refer [is= is]])
  (:require [database :refer [state create-initial-state! get-cards-as-strings get-suit-number get-card-value]])
  )


(defonce hand-size 8)

(defn important []
  (println "Älskar dig Pappa")
  )


(defn toggle-menu! [state]
  (swap! state assoc :show_menu (if (:show_menu @state)
                                  false
                                  true)))

(declare discard!)
(defn add-to-score!
  ;Adds value to score
  [state value]
  (swap! state assoc :score (+ value (:score @state))))

(defn get-score
  ;calculates the score of a play. See balatro-scorecard.jpeg
  {:test (fn []
           (is= (get-score [101 201 310 411 412]) 10)
           (is= (get-score [101 201 302 402 412]) 20)
           )}
  [cards-played]
  ;ToDo
  ; Check if hand matches the highest ranking hand criteria first, ex royal flush, and progress to lower ranks
  ; Implement functions for each hand that return the score or 0 if conditions are not met
  ; Start with something easy like pair, three of a kind and flush using built-in function frequencies
  10
  )


(defn deal! [state]
  (let [missing (- hand-size (count (:hand @state)))]
    (swap! state assoc :hand (into [] (sort (concat (:hand @state) (take missing (:deck @state))))))
    (swap! state assoc :deck (subvec (:deck @state) missing))))

(defn play! [state cards-to-play]
  (let [chosen-cards (->> (:hand @state)
                          (map-indexed vector)
                          (filter (fn [[k _]]
                                    (some #(= (+ k 1) %) cards-to-play)))
                          (reduce (fn [a v]
                                    (conj a (second v)))
                                  []))]
    (println "You played: " (get-cards-as-strings chosen-cards))
    (add-to-score! state (get-score chosen-cards))
    (discard! state cards-to-play)))

(defn select-cards
  ;Returns a vector with the cards the user wants to do "action" with
  [action]
  (print "Cards to " action ": ")
  (flush)
  (reduce (fn [a v]
            (conj a (Integer/parseInt v)))
          []
          (str/split (read-line) #"\s+")))

(defn discard
  ;;discards the cards in cards-to-discard from hand
  {:test (fn []
           (is= (discard ["C1" "C2" "C3" "C4" "C5"] [1 3 5]) ["C2" "C4"])
           (is= (discard ["C1" "C2" "C3" "C4" "C5"] [1 3 5 6]) ["C2" "C4"])
           (is= (discard ["C1" "C2" "C3"] []) ["C1" "C2" "C3"])
           (is= (discard [] [1 3 5]) []))}
  [hand cards-to-discard]
  (->> hand
       (map-indexed vector)
       (remove (fn [[k _]]
                 (some #(= (+ k 1) %) cards-to-discard)))
       (reduce (fn [a v]
                 (conj a (second v)))
               [])))

(defn discard! [state cards-to-discard]
  (swap! state assoc :hand (discard (:hand @state) cards-to-discard)))


(defn -main [& args]
  (println "Let's play! \u2660")
  (create-initial-state!)
  (deal! state)
  (loop [count 1]
    (println "Score" (:score @state))
    (println "Round" count (get-cards-as-strings (:hand @state)))
    (if (:show_menu @state)
      (println "q - quit\np - play\nd - discard\nt - toggle menu"))
    (case (read-line)
      "q" nil
      "p" (do (play! state (select-cards "play"))
              (deal! state)
              (recur (inc count)))
      "d" (do (discard! state (select-cards "discard"))
              (deal! state)
              (recur count))
      "t" (do (toggle-menu! state)
              (recur count))
      (do (println "Invalid command")
          (recur count)))))

(comment
  (-main)
  )



