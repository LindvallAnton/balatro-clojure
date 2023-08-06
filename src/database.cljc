(ns database)
(def state (atom {}))

(defn create-deck []
  (into [] (for [s [100 200 300 400]
                 n [1 2 3 4 5 6 7 8 9 10 11 12 13]]
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
