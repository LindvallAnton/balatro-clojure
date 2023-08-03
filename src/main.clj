(ns main
  (:require [clojure.set :as s])
  (:require [java-time.api :as t])
  (:require [clojure.string :as str])
  (:require [ysera.test :refer [is= is]])
  )

(defn create-deck []
  (into [] (for [s ['C 'D 'H 'S]
                 n ['A 2 3 4 5 6 7 8 9 10 'J 'Q 'K]]
             (str s n))))

(defonce handsize 8)

(def state (atom {}))

(defn init-state! [deck hand choices]
  (swap! state assoc :deck deck)
  (swap! state assoc :hand hand)
  (swap! state assoc :choices choices)
  )

(defn important []
  (println "Älskar dig Pappa")
  )

(defn create-initial-state []
  (init-state! (shuffle (create-deck)) [] [])
  )

(defn shuffle-deck! [state]
  (swap! state assoc :deck (shuffle (:deck @state))))

(defn deal! [state]
  (let [missing (- handsize (count (:hand @state)))]
    (swap! state assoc :hand (into [] (concat (:hand @state) (take missing (:deck @state)))))
    (swap! state assoc :deck (subvec (:deck @state) missing))
    ))

(defn play []
  ;;ToDo Make it possible to play more than one card :-)
  (println (:hand @state))
  (let [user-input (read-line)
        str-vect (str/split user-input #"\s+")]
    (println (user-input))
    (print (nth (:hand @state) (- (Integer/parseInt (first str-vect)) 1)))
    ))

(defn discard! [state cards-to-discard]
  ;;discards the cards in coll cards-to-discard from :hand
  (swap! state assoc :hand
         (->> (:hand @state)
              (map-indexed vector)
              (remove (fn [[k v]]
                        (some #(= (+ k 1) %) cards-to-discard)))
              (reduce (fn [a v]
                        (conj a (second v)))
                      []))
         ))

(comment
  (create-initial-state)

  ;;ToDo Move to test of deal!
  (init-state! (create-deck) ["P1" "P2"] [])
  (init-state! (create-deck) [] [])
  (print @state)
  (deal! state)
  (print @state)
  (play)
  (shuffle-deck! state)

  ;;Discard lab
  (map-indexed vector (:hand @state))
  (filter odd? [0 1 2 3 4 5 6 7])
  (remove odd? [0 1 2 3 4 5 6 7])

  ;;Remove
  (remove (fn [[k v]]
            (some #(= k %) [1 3 5]))
          (map-indexed vector (:hand @state)))

  (reduce (fn [a v]
            (conj a (first v)))
          []
          [[0 10] [1 11] [2 12]]
          )

  (discard! state [3 4 5])
  (swap! state assoc :deck (s/difference (set (:deck @state)) (set (:hand @state))))
  )



