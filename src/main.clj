(ns main
  (:require [clojure.string :as str])
  (:require [ysera.test :refer [is= is]])
  (:require [database :refer [state create-initial-state!]])
  )


(defonce hand-size 8)

(defn important []
  (println "Älskar dig Pappa")
  )


(defn toggle-menu! []
  (swap! state assoc :show_menu (if (:show_menu @state)
                                  false
                                  true)))

(declare discard!)
(declare get-cards-as-strings)
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
                                  [])
                          )]
    (println "You played: " (get-cards-as-strings chosen-cards))
    ;;ToDo count score
    (discard! state cards-to-play)))

(defn select-cards [action]
  ;Returns a vector with the cards the user wants to do "action" with
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

(defn get-suit-number [card]
  (int (Math/floor (/ card 100))))

(defn get-card-value [card]
  (- card (* 100 (get-suit-number card))))
(defn get-cards-as-strings [cards]
  (->> cards
       (reduce (fn [a v]
                 (conj a (str (case (get-suit-number v)
                                1 "S"
                                2 "D"
                                3 "C"
                                4 "H"
                                )
                              (case (get-card-value v)
                                1 "A"
                                11 "J"
                                12 "Q"
                                13 "K"
                                (get-card-value v)
                                ))))
               [])))


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
      "t" (do (toggle-menu!)
              (recur count))
      (do (println "Invalid command")
          (recur count)))))

  (comment
    (-main)
    )



