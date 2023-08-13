(ns scoring
  (:require [ysera.test :refer [is=]])
  (:require [database :refer [get-suit-number get-card-rank get-card-ranks]])
  )

(def scores {:royal-flush     {:name "Royal Flush" :base-score 200 :multiplier 10}
             :straight-flush  {:name "Straight Flush" :base-score 100 :multiplier 8}
             :four-of-a-kind  {:name "Four of a Kind" :base-score 60 :multiplier 5}
             :full-house      {:name "Full house" :base-score 40 :multiplier 4}
             :flush           {:name "Flush" :base-score 40 :multiplier 3}
             :straight        {:name "Straight" :base-score 30 :multiplier 3}
             :three-of-a-kind {:name "Three of a Kind" :base-score 20 :multiplier 3}
             :two-pair        {:name "Two pair" :base-score 20 :multiplier 2}
             :pair            {:name "Pair" :base-score 10 :multiplier 2}
             :high-card       {:name "High Card" :base-score 5 :multiplier 1}
             })

;; Source for Balatro scoring rules https://forums.somethingawful.com/showthread.php?threadid=4034530

(defn pair?
  ;;returns true if cards have exactly one pair
  {:test (fn []
           (is= (pair? [101 201 311 312 313]) true)
           (is= (pair? [111 211]) true)
           (is= (pair? [101 201 102 202 405]) false)        ;;test case is not pair, it is two-pair
           )}
  [cards]
  (= 1 (count
         (->> cards
              (get-card-ranks)
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
  (true?                                                    ;;to avoid returning nil instead of false if not true
    (->> cards
         (get-card-ranks)
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
  (true?                                                    ;;to avoid returning nil instead of false if not true
    (->> cards
         (get-card-ranks)
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
              (get-card-ranks)
              (frequencies)
              (vals)
              (filter #(= 2 %))
              ))))

(defn straight?
  ;;returns true if all cards in hand are in a consecutive sequence
  {:test (fn []
           (is= (straight? [114 202 303 404 105]) true)     ;;Ace can be used as 1 or 14
           (is= (straight? [110 211 312 413 114]) true)     ;;Ace can be used as 1 or 14
           (is= (straight? [103 204 305 406]) false)        ;;has to be five consecutive cards
           (is= (straight? [113 114 102 103 104]) false)    ;;straights cannot wrap around
           (is= (straight? [305 204 406 103 207]) true)
           (is= (straight? [305 206 407 108 210]) false)
           )}
  [cards]
  (let [values (sort (get-card-ranks cards))]
    (or (= values [2 3 4 5 14])
        (and (= 4 (- (apply max values) (apply min values)))
             (= 5 (count (set values)))))))
(defn flush?
  ;;returns true if five cards are in the same suit
  {:test (fn []
           (is= (flush? [313 308 303 304 311]) true)
           (is= (flush? [313 308 303 304]) false)           ;;has to be five cards
           (is= (flush? [313 308 303 304 211]) false)
           )}
  [cards]
  (let [suits (->> cards
                   (reduce (fn [a v]
                             (conj a (get-suit-number v)))
                           []))]
    (and (= 1 (count (set suits))) (= 5 (count suits)))))

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
  ;;returns true if five cards are in a sequence and in the same suit and top card is Ace
  {:test (fn []
           (is= (royal-flush? [405 408 407 409 406]) false)
           (is= (royal-flush? [410 411 412 413 414]) true)
           (is= (royal-flush? [411 412 413 414]) false)     ;;has to be five cards
           (is= (royal-flush? [311 310 312 314 313]) true)
           (is= (royal-flush? [311 310 312 314 313]) true)
           (is= (royal-flush? [105 106 112 205 201]) false)
           )}
  [cards]
  (let [values (get-card-ranks cards)]
    (and (straight? cards) (flush? cards) (= 14 (apply max values)))))

(defn full-house?
  ;;returns true if cards
  {:test (fn []
           (is= (full-house? [101 201 301 404 304]) true)
           (is= (full-house? [101 201 303 404 304]) false)
           (is= (full-house? [101 201 301 404 305]) false)  ;;the cards counted as three of a kind must not be mistaken to be the pair
           )}
  [cards]
  (and (pair? cards) (three-of-a-kind? cards)))

(defn get-play-classification
  ;returns what type of play cards represents, e.g. pair, straight or full house
  {:test (fn []
           (is= (get-play-classification [410 411 412 413 414]) :royal-flush)
           (is= (get-play-classification [101 201 310 411 412]) :pair))}
  [cards]
  (cond
    (royal-flush? cards) :royal-flush
    (straight-flush? cards) :straight-flush
    (four-of-a-kind? cards) :four-of-a-kind
    (full-house? cards) :full-house
    (flush? cards) :flush
    (straight? cards) :straight
    (three-of-a-kind? cards) :three-of-a-kind
    (two-pair? cards) :two-pair
    (pair? cards) :pair
    :else :high-card))

(defn get-score-for-rank
  ;returns the score for a card rank
  {:test (fn []
           (is= (get-score-for-rank 4) 4)
           (is= (get-score-for-rank 12) 10))}
  [card-rank]
  (case card-rank
    (2 3 4 5 6 7 8 9) card-rank
    (10 11 12 13 14) 10))

(defn get-score-for-ranks
  ;returns the score for a collection of card values
  [card-ranks]
  (->> card-ranks
       (reduce (fn [ack val]
                 (+ ack (get-score-for-rank val)))
               0)))
(defn calculate-value-of-cards
  ;return the card values for the cards contributing to the hand,
  ;e.g. in a five card hand with a pair, only the value of the two cards in the pair contributes to the score
  {:test (fn []
           (is= (calculate-value-of-cards :high-card [102 103 205 206 311]) 10)
           (is= (calculate-value-of-cards :pair [107 207 308 409]) (+ 7 7))
           (is= (calculate-value-of-cards :two-pair [107 207 208 308 412]) (+ 7 7 8 8))
           (is= (calculate-value-of-cards :three-of-a-kind [107 207 307 308]) (+ 7 7 7))
           (is= (calculate-value-of-cards :four-of-a-kind [107 207 307 407 109]) (+ 7 7 7 7))
           (is= (calculate-value-of-cards :flush [107 108 103 102 114]) (+ 7 8 3 2 10)))}
  [play-classification cards]
  (case play-classification
    :high-card (->> cards
                    (get-card-ranks)
                    (apply max)
                    (get-score-for-rank))
    (:pair :two-pair) (->> cards
                           (get-card-ranks)
                           (frequencies)
                           (filter #(= 2 (val %)))
                           (reduce (fn [ack val]
                                     (conj ack (first val)))
                                   [])
                           (get-score-for-ranks)
                           (* 2))
    :three-of-a-kind (->> cards
                          (get-card-ranks)
                          (frequencies)
                          (filter #(= 3 (val %)))
                          (flatten)
                          (first)
                          (get-score-for-rank)
                          (* 3))
    :four-of-a-kind (->> cards
                         (get-card-ranks)
                         (frequencies)
                         (filter #(= 4 (val %)))
                         (flatten)
                         (first)
                         (get-score-for-rank)
                         (* 4))
    (:straight :flush
      :straight-flush
      :royal-flush
      :full-house) (->> cards
                        (get-card-ranks)
                        (get-score-for-ranks))))

(comment

  (->> [12 12 13 13 4]
       (frequencies)
       (filter #(= 2 (val %)))
       (reduce (fn [ack val]
                 (conj ack (first val)))
               [])
       (get-score-for-ranks)
       (* 2))
  )
(defn get-score
  ;calculates the score of a play. See balatro-scorecard.jpeg
  {:test (fn []
           (is= (get-score [102 202]) (* 2 (+ 10 2 2)))     ; pair
           (is= (get-score [102 202 310 411 412]) (* 2 (+ 10 2 2))) ;pair
           (is= (get-score [102 202 303 403]) (* 2 (+ 20 2 2 3 3))) ;two-pair
           (is= (get-score [102 202 303 403 412]) (* 2 (+ 20 2 2 3 3))) ;two-pair
           (is= (get-score [114 214 314 106 206]) (* 4 (+ 40 10 10 10 6 6))) ;full-house
           )}
  [cards]
  (let [play-classification (get-play-classification cards)
        base-score (get-in scores [play-classification :base-score])
        multiplier (get-in scores [play-classification :multiplier])]
    ;    (println play-classification base-score "x" multiplier)
    (* multiplier (+ base-score (calculate-value-of-cards play-classification cards)))
    ))
