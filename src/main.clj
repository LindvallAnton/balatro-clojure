(ns main
  (:require [clojure.set :as s])
  ;;  (:require [java-time.api :as t])
  (:require [clojure.string :as str])
  (:require [ysera.test :refer [is= is]])
  )

(defn create-deck []
  (into [] (for [s ['C 'D 'H 'S]
                 n ['A 2 3 4 5 6 7 8 9 10 'J 'Q 'K]]
             (str s n))))

(defonce hand-size 8)

(def state (atom {}))

(defn init-state! [deck hand choices]
  (swap! state assoc :deck deck)
  (swap! state assoc :hand hand)
  (swap! state assoc :choices choices)
  (swap! state assoc :show_menu true)
  )

(defn important []
  (println "Älskar dig Pappa")
  )

(defn create-initial-state! []
  (init-state! (shuffle (create-deck)) [] [])
  )

(defn shuffle-deck! [state]
  (swap! state assoc :deck (shuffle (:deck @state))))

(defn toggle-menu! []
  (swap! state assoc :show_menu (if (:show_menu @state)
                                  false
                                  true)))

(declare discard!)
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
    (println "You played: " chosen-cards)
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

(defn -main [& args]
  (println "Let's play! \u2660")
  (create-initial-state!)
  (deal! state)
  (loop []
    (println "Hand:" (:hand @state))
    (if (:show_menu @state)
      (println "q - quit\np - play\nd - discard\nt - toggle menu"))
    (case (read-line)
      "q" nil
      "p" (do (play! state (select-cards "play"))
              (deal! state)
              (recur))
      "d" (do (discard! state (select-cards "discard"))
              (deal! state)
              (recur))
      "t" (do (toggle-menu!)
              (recur))
      (do (println "Invalid command")
          (recur)))))

(comment
  (-main)
  )



