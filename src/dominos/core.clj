(ns dominos.core
  (:gen-class)
  (:require [taoensso.timbre :refer [debug error spy]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def dominos [{:top 0 :bottm 0}
              {:top 0 :bottm 1 :picked false}
              {:top 1 :bottm 1 :picked false}
              {:top 0 :bottm 2 :picked false}
              {:top 1 :bottm 2 :picked false}
              {:top 2 :bottm 2 :picked false}
              {:top 0 :bottm 3 :picked false}
              {:top 1 :bottm 3 :picked false}
              {:top 2 :bottm 3 :picked false}
              {:top 3 :bottm 3 :picked false}
              {:top 0 :bottm 4 :picked false}
              {:top 1 :bottm 4 :picked false}
              {:top 2 :bottm 4 :picked false}
              {:top 3 :bottm 4 :picked false}
              {:top 4 :bottm 4 :picked false}
              {:top 0 :bottm 5 :picked false}
              {:top 1 :bottm 5 :picked false}
              {:top 2 :bottm 5 :picked false}
              {:top 3 :bottm 5 :picked false}
              {:top 4 :bottm 5 :picked false}
              {:top 5 :bottm 5 :picked false}
              {:top 0 :bottm 6 :picked false}
              {:top 1 :bottm 6 :picked false}
              {:top 2 :bottm 6 :picked false}
              {:top 3 :bottm 6 :picked false}
              {:top 4 :bottm 6 :picked false}
              {:top 5 :bottm 6 :picked false}
              {:top 6 :bottm 6 :picked false}])

(defn new-board []
  (set (vec (map (fn [el]
                   [(:top el) (:bottm el)]) dominos))))

(defn play-domino [current-board player-stack]
  (let [new-board (conj current-board (first player-stack))]

    new-board))

(defn create-new-game [player1 player2]
  (let [possible-tiles (clojure.set/difference (new-board) (-> player1 :tiles) (-> player2 :tiles))
        rand-idx (rand-int 15)]

    {:player1 player1
     :player2 player2
     :board [(-> possible-tiles vec (get rand-idx))]}))

(defn select-domino [player-stack join-start join-end]
  (let [possible-tiles-for-end (vec (filter (fn [x]
                                              (let [joining-half (last join-end)]
                                                (contains? (set x) joining-half))) (vec player-stack)))

        possible-tiles-for-start (vec (filter (fn [x]
                                                (let [joining-half (first join-start)]
                                                  (contains? (set x) joining-half))) (vec player-stack)))]

    (if-not (nil? (first possible-tiles-for-end))
      {:tile (first possible-tiles-for-end)
       :position :end}
      {:tile (first possible-tiles-for-start)
       :position :start})))

(defn update-board [current-board domino player-name]
  (let [{:keys [tile position]} domino
        joining-board-number (if (= position :start)
                               (ffirst current-board)
                               (-> current-board last last))

        rearranged-tile (cond (= position :start) (if (= (last tile) joining-board-number)
                                                    tile
                                                    (-> tile reverse vec))

                              (= position :end) (if (= (first tile) joining-board-number)
                                                  tile
                                                  (-> tile reverse vec)))

        msg (if (empty? rearranged-tile)
              (str player-name " can't play to connect " (first current-board))

              (if (= position :start)
                (str player-name " plays " rearranged-tile " to connect to tile " (first current-board))
                (str player-name " plays " rearranged-tile " to connect to tile " (last current-board))
                ))]



    (println msg)

    (if-not (nil? tile)
      (if (= position :start)
        (vec (concat [rearranged-tile] (vec current-board)))
        (vec (conj current-board rearranged-tile)))

      current-board)))

(defn play [current-board player-key]
  (let [player (-> current-board player-key)
        selected-domino (select-domino (-> player :tiles) (-> current-board :board first) (-> current-board :board last))
        remaining-tiles (disj (-> current-board player-key :tiles) (:tile selected-domino))
        updated-player (merge player {:tiles remaining-tiles})
        new-board (update-board (:board current-board) selected-domino (:name player))
        update {:board (vec (remove nil? new-board))
                player-key updated-player}
        new-board (merge current-board  update)]

    (println "Board is now :" (-> new-board :board))

    new-board))

(defn play-dominos []
  (let [shuffuled_tiles (shuffle (new-board))
        tiles1 (set (vec (take 7 shuffuled_tiles)))
        tiles2 (set (vec (take-last 7 shuffuled_tiles)))
        player1 {:tiles tiles1
                 :name "Alice"
                 :winner false
                 :playing true}
        player2 {:tiles tiles2
                 :winner false
                 :name "Bob"
                 :playing false}
        current_player (rand-int 2)

        board (atom (create-new-game player1  player2))
        current-player (atom :player1)
        i (atom 17)]

    (while (and (> (-> @board :player1 :tiles count) 0)
                (> (-> @board :player2 :tiles count) 0)
                (> @i 0))

      (swap! board (fn [n]
                     (play n :player1)))


      (swap! board (fn [n]
                     (play n :player2)))

      (swap! i dec))

    (let [final-players [(-> @board :player1) (-> @board :player2)]
          winner (first (filter (fn [el] (= (-> el :tiles count) 0)) final-players))]

      (if winner
        (println "The winner is : " (:name winner))
        (println "The game ended in a draw"))

      @board)
    ))
