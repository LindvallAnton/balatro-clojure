(ns main
  (:require [clojure.set :as s])
  (:require [java-time.api :as t])
  (:require [clojure.string :as str])
  (:require [ysera.test :refer [is= is]])
  )

(defn create-deck []
  (into [] (for [s ['C 'D 'H 'S]
                 n ['A 2 3 4 5 6 7 8 9 10 'J 'Q 'K]]
             [s n])))

(defonce handsize 8)

(def state (atom {}))

(defn init-state! [deck hand choices]
  (swap! state assoc :deck deck)
  (swap! state assoc :hand hand)
  (swap! state assoc :choices choices)
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
  ;remove selected cards from hand, can't be nothing, then deal.
  (println (:hand @state))
  (let [user-input (read-line)
        str-vect (str/split user-input #" ")]
    (print (nth (:hand @state) (- (Integer/parseInt (first str-vect)) 1)))
    ))

(comment
  (create-initial-state)

  ;;ToDo Move to test of deal!
  (init-state! (create-deck) [['P 1] ['P 2]] [])
  (print @state)
  (deal! state)
  (print @state)
  (play)
  (shuffle-deck! state)

  (swap! state assoc :deck (s/difference (set (:deck @state)) (set (:hand @state))))
  )



