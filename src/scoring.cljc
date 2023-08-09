(ns scoring
  (:require [ysera.test :refer [is=]])
  (:require [database :refer [get-suit-number get-card-value]])
  )

(def scores {:royal-flush 200
             :straight-flush 100
             :four-of-a-kind 60
             :full-house 40
             :flush 40
             :straight 30
             :three-of-a-kind 20
             :two-pair 20
             :pair 10
             })

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
  ;;returns true if all cards in hand are in a consecutive sequence
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
  ;;returns true if all cards in hand are in the same suit
  {:test (fn []
           (is= (flush? [313 308 303 304 311]) true)
           (is= (flush? [313 308 303 304 211]) false)       ;;straights cannot wrap around the top
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
  ;;returns true if all cards in hand are in a sequence and in the same suit and
  {:test (fn []
           (is= (royal-flush? [405 408 407 409 406]) false)
           (is= (royal-flush? [410 411 412 413 414]) true)
           (is= (royal-flush? [311 310 312 314 313]) true)
           (is= (royal-flush? [311 310 312 314 313]) true)
           (is= (royal-flush? [105 106 112 205 201]) false)
           )}
  [cards]
  (let [values (->> cards
                    (reduce (fn [a v]
                              (conj a (get-card-value v)))
                            [])
                    )]
    (and (straight? cards) (flush? cards) (= 14 (apply max values)))))

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
           (is= (get-score [101 201 310 411 412]) (:pair scores))
           (is= (get-score [101 201 302 402 412]) (:two-pair scores)))}
  [cards]
  (cond
    (royal-flush? cards) (:royal-flush scores)
    (straight-flush? cards) (:straight-flush scores)
    (four-of-a-kind? cards) (:four-of-a-kind scores)
    (full-house? cards) (:full-house scores)
    (flush? cards) (:flush scores)
    (straight? cards) (:straight scores)
    (three-of-a-kind? cards) (:three-of-a-kind scores)
    (two-pair? cards) (:two-pair scores)
    (pair? cards) (:pair scores)
    :else 0))
