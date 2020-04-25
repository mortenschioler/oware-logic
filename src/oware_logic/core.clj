(ns oware-logic.core
  "A game is to a position what identity is to state."
  (:require [clojure.string :as str]))

(def starting-position
  {:active-player 0
   :seeds         (vec (repeat 12 4))
   :score         [0 0]})

(defn field
  [player]
  (case player
    0 #{0 1 2 3 4 5}
    1 #{6 7 8 9 10 11}))

(defn seeds-for-player
  [player seeds]
  (nth (partition 6 seeds) player))

(defn owns?
  [player pit]
  (contains? (field player) pit))

(defn opponent
  [player]
  (case player
    1 0
    0 1))

(defn move-decapitates?
  "In Oware Abapa, a move that removes all seeds from the opponent's field
  is said to 'decaptitate' them, and such a move is illegal."
  [position-after-harvesting]
  (->> (:seeds position-after-harvesting)
       (seeds-for-player (opponent (:active-player position-after-harvesting)))
       (reduce +)
       (= 0)))

(defn next-pit
  [pit]
  (mod (inc pit) 12))

(defn ripe-for-harvest?
  [active-player pit num-seeds]
  (and (#{2 3} num-seeds)
       (not (owns? active-player pit))))

(defn mark-ripe-for-harvest
  [position pit]
  (update position :ripe (fnil conj #{}) pit))

(defn sow-one
  [position pit]
  (let [num-seeds (inc (get-in position [:seeds pit]))]
    (cond-> (assoc-in position [:seeds pit] num-seeds)
            (ripe-for-harvest? (:active-player position) pit num-seeds)
            (mark-ripe-for-harvest pit))))

(defn sow
  [position origin-pit]
  (loop [current-position (assoc-in position [:seeds origin-pit] 0)
         pit-to-sow (next-pit origin-pit)
         remaining-seeds-in-hand (get-in position [:seeds origin-pit])]
    (cond
      (zero? remaining-seeds-in-hand)
      current-position

      (= origin-pit pit-to-sow)
      (recur
        current-position
        (next-pit pit-to-sow)
        remaining-seeds-in-hand)

      :else
      (recur
        (sow-one current-position pit-to-sow)
        (next-pit pit-to-sow)
        (dec remaining-seeds-in-hand)))))

(defn harvest-one
  [position pit]
  (-> position
      (assoc-in [:seeds pit] 0)
      (update-in [:score (:active-player position)] (partial + (get-in position [:seeds pit])))))

(defn harvest
  [position]
  (-> (reduce
        harvest-one
        position
        (:ripe position))
      (dissoc :ripe)))

(defn pass-turn
  [position]
  (update position :active-player opponent))

(defn check-draw
  [position]
  (let [{:keys [score]} position]
    (if (= 24 (first score) (second score))
      (assoc position :outcome :draw)
      position)))

(defn check-win
  [position]
  (let [{:keys [score]} position
        [score0 score1] score]
    (cond-> position
            (>= score0 25)
            (assoc :outcome 0)

            (>= score1 25)
            (assoc :outcome 1))))

(defn check-outcome
  [position]
  (-> position
      check-draw
      check-win))

(defn move
  [position pit]
  (let [{:keys [active-player]} position
        position-after-move (-> position
                                (sow pit)
                                (harvest)
                                (check-outcome)
                                (pass-turn))]
    (cond
      (contains? position :outcome)
      (throw (ex-info "Game is over."
                      {:position position}))

      (not (owns? active-player pit))
      (throw (ex-info "Illegal move: Must take from own field."
                      {:position position
                       :pit pit}))

      (move-decapitates? position-after-move)
      (throw (ex-info "Illegal move: Must not take away opponents last legal move."
                      {:position position
                       :pit pit
                       :position-after-move position-after-move}))

      :else position-after-move)))

(def bullet \u2022)

(defn ->str
  [position]
  (let [{:keys [seeds active-player score]} position]
    (str/join "\n"
              [(str (second score) (when (= active-player 1) bullet))
               (str/join " " (map seeds [11 10 9 8 7 6]))
               (str/join " " (map seeds [0 1 2 3 4 5]))
               (str (first score) (when (= active-player 0) bullet))])))

(defn show
  [position]
  (println (->str position)))
