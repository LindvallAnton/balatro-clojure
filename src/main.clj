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

(defn pair?
  ;;returns true if cards have exactly one pair
  {:test (fn []
           (is= (pair? [101 201 311 312 313]) true)
           (is= (pair? [101 201 102 202 405]) false)        ;;test case is not pair, it is two-pair
           )}
  [cards]
  (= 1 (count
         (->> cards
              (reduce (fn [a v]
                        (conj a (get-card-value v)))
                      [])
              (frequencies)
              (vals)
              (filter #(= 2 %))
              ))))
(defn three-of-a-kind?
  ;;returns true if cards fulfill requirement of three-of-a-kind
  {:test (fn []
           (is= (three-of-a-kind? [101 201 301 312 313]) true)
           (is= (three-of-a-kind? [101 201 203 303 405]) false)
           )}
  [cards]
  (= true                                                   ;;to avoid returning nil instead of false if not true
     (->> cards
          (reduce (fn [a v]
                    (conj a (get-card-value v)))
                  [])
          (frequencies)
          (vals)
          (some #(= 3 %))
          )))
(defn four-of-a-kind?
  ;;returns true if cards fulfill requirement of three-of-a-kind
  {:test (fn []
           (is= (four-of-a-kind? [101 201 301 401 313]) true)
           (is= (four-of-a-kind? [101 201 301 303 405]) false)
           )}
  [cards]
  (= true                                                   ;;to avoid returning nil instead of false if not true
     (->> cards
          (reduce (fn [a v]
                    (conj a (get-card-value v)))
                  [])
          (frequencies)
          (vals)
          (some #(= 4 %))
          )))

(defn two-pair?
  ;;returns true if cards fulfill requirement of pair
  ;;the pairs need to be of different value, otherwise it is four of a kind
  {:test (fn []
           (is= (two-pair? [101 201 302 402 313]) true)
           (is= (two-pair? [101 201 202 402 313]) true)
           (is= (two-pair? [101 202 402 313 201]) true)
           (is= (two-pair? [101 201 203 204 405]) false)
           )}
  [cards]
  (= 2 (count
         (->> cards
              (reduce (fn [a v]
                        (conj a (get-card-value v)))
                      [])
              (frequencies)
              (vals)
              (filter #(= 2 %))
              ))))

(defn straight?
  ;;returns true if all cards in hand are in the same suit
  {:test (fn []
           (is= (straight? [101 202 303 404 105]) true)
           (is= (straight? [113 101 102 103 104]) false)    ;;straights cannot wrap around the top
           (is= (straight? [305 204 406 103 207]) true)
           )}
  [cards]
  (let [values (->> cards
                    (reduce (fn [a v]
                              (conj a (get-card-value v)))
                            [])
                    )]
    (and (= 4 (- (apply max values) (apply min values)))
         (= 5 (count (set values))))
    ))
(defn flush?
  ;;returns true if all cards in hand are in a sequence
  {:test (fn []
           (is= (flush? [313 308 303 304 311]) true)
           (is= (flush? [313 308 303 304 211]) false)    ;;straights cannot wrap around the top
           )}
  [cards]
  (let [suits (->> cards
                    (reduce (fn [a v]
                              (conj a (get-suit-number v)))
                            [])
                    )]
    (= 1 (count (set suits)))))

(defn straight-flush?
  ;;returns true if all cards in hand are in a sequence and in the same suit
  {:test (fn []
           (is= (straight-flush? [405 408 407 409 406]) true)
           (is= (straight-flush? [407 408 409 410 412]) false)
           (is= (straight-flush? [407 408 409 410 311]) false)
           )}
  [cards]
  (and (straight? cards) (flush? cards)))
(defn royal-flush?
  ;;returns true if all cards in hand are in a sequence and in the same suit
  {:test (fn []
           (is= (royal-flush? [405 408 407 409 406]) false)
           (is= (royal-flush? [410 411 412 413 414]) true)
           (is= (royal-flush? [311 310 312 314 313]) true)
           )}
  [cards]
  (let [values (->> cards
                    (reduce (fn [a v]
                              (conj a (get-card-value v)))
                            [])
                    )]
  (and (straight? cards) (flush? cards)) (= 14 (apply max values))))

(defn full-house?
  ;;returns true if all cards in hand are in a sequence and in the same suit
  {:test (fn []
           (is= (full-house? [101 201 301 404 304]) true)
           (is= (full-house? [101 201 303 404 304]) false))}
  [cards]
  (and (pair? cards) (three-of-a-kind? cards)))

(defn get-score
  ;calculates the score of a play. See balatro-scorecard.jpeg
  {:test (fn []
           (is= (get-score [101 201 310 411 412]) 10)
           (is= (get-score [101 201 302 402 412]) 20)
           (is= (get-score [101 202 307 408 112]) 0)
           )}
  [cards]
  (cond
    (royal-flush? cards) 200
    (straight-flush? cards) 100
    (four-of-a-kind? cards) 60
    (full-house? cards) 40
    (flush? cards) 40
    (straight? cards) 30
    (three-of-a-kind? cards) 20
    (two-pair? cards) 20
    (pair? cards) 10
    :else 0))

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



