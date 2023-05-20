(ns ttt
  (:require [clojure.string :as cstr]))

(def game {:current :p1
           :next    :p2
           :moves   {:p1 #{}
                     :p2 #{}}})

(def winning-combos [#{1 2 3} #{4 5 6} #{7 8 9} #{1 4 7}
                     #{2 5 8} #{3 6 9} #{1 5 9} #{3 5 7}])

(defn get-moves [game player] ((game :moves) (game player)))

(defn win? [moves]
  (some #(every? moves %) winning-combos))

(defn draw? [game] (= (+ (count (get-moves game :next)) (count (get-moves game :current))) 9))

(defn game-over? [game] (cond
                          (win? (get-moves game :next)) :win
                          (draw? game) :draw
                          :else :keep-playing))

(defn fill-board [moves symbol board]
  (map-indexed #(if (contains? moves (inc %1)) symbol %2) board))

(defn generate-board [game]
  (cstr/join "--+---+--\n"
             (map #(cstr/join " " (interleave  % '("|" "|" "\n")))
                  (partition 3
                             (fill-board ((game :moves) :p1)
                                         "X"
                                         (fill-board ((game :moves) :p2)
                                                     "O"
                                                     (repeat 9 " ")))))))

(loop [game game]
  (println (generate-board game))
  (if (not (= (game-over? game) :keep-playing))
    (println "game over!" (if (= (game-over? game) :win)
                            (str (name (game :next)) " won!")
                            "draw"))
    (-> game
        (assoc-in [:moves (game :current)] (conj (get-moves game :current) (read-string (read-line))))
        (assoc :current (game :next) :next (game :current))
        recur)))
