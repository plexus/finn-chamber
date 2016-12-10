(ns finn-chamber.core
  (:require [cljsjs.phaser]
            [finn-chamber.math :refer [abs]]
            [finn-chamber.wrapper :refer [load-image
                                          add-tile-sprite
                                          add-sprite
                                          add-game-state
                                          start-game-state
                                          start-physics-system!
                                          create-group
                                          set-gravity
                                          set-immutable
                                          set-velocity-x
                                          set-velocity-y
                                          set-anchor
                                          physics-collide
                                          create-cursor-keys
                                          keyboard-add-key
                                          key-down?]]
            [finn-chamber.levels :as levels]))


(enable-console-print!)

(defonce db (atom {:level levels/level-1
                   :game (js/Phaser.Game. 1280 720)}))

(def floor-positions [0 7 14 21 29 37 44])

(keys (:levers (:level @db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-background [game]
  (add-tile-sprite game 0 0 1280 720 "background-tiles"))

(defn draw-floor-tile [game group x y]
  (let [floor (add-sprite game (* x 16) (* y 16) "floor")]
    (.add group floor)
    (set-immutable floor true)
    floor))

(defn draw-floors [{:keys [game level] :as db}]
  (let [spec (:floors level)
        group (create-group game)]
    (doall (map (fn [y tiles]
                  (doall (map-indexed (fn [x yes-no]
                                        (when yes-no (draw-floor-tile game group x y)))
                                      tiles))
                  ) floor-positions spec))
    (assoc-in db [:groups :floor] group)))

(defn item-pos-x [x]
  (* x 16))

(defn item-pos-y [y]
  (* (nth floor-positions (inc y)) 16))

(defn create-lever-sprite [game x y angle]
  (let [lever (add-sprite game (item-pos-x x) (item-pos-y y) "lever")]
    (set! (.-angle lever) angle)
    (set-anchor lever 0.5 1)
    lever))

(defn create-lever-base-sprite [game x y]
  (let [base (add-sprite game (item-pos-x x) (item-pos-y y) "lever_base")]
    (set-anchor base 0.5 1)
    base))

(defn draw-levers [{{:keys [levers]} :level game :game :as db}]
  (reduce (fn [db lever-id]
            (let [{:keys [x y]} (get levers lever-id)
                  lever (create-lever-sprite game x y -45)
                  lever-base (create-lever-base-sprite game x y)]
              (assoc-in db [:level :levers lever-id :sprite] lever)
              (assoc-in db [:level :levers lever-id :base] lever-base))
            ) db (keys levers)))

(defn create-finn [game]
  (let [finn (add-sprite game 900 (+ 16 48) "finn")]
    (.setTo (.-scale finn) 1.5)
    (set-anchor finn 0.5 0.5)
    (set-gravity finn 600)
    finn))

(defn sprite-scale-x [sprite amount]
  (set! (.. sprite -scale -x) amount))

(defn finn-go-left [finn]
  (sprite-scale-x finn (abs (.. finn -scale -x)))
  (set-velocity-x finn -200))

(defn finn-go-right [finn]
  (sprite-scale-x finn (* -1 (abs (.. finn -scale -x))))
  (set-velocity-x finn 200))

(defn finn-jump [finn]
  (set-velocity-y finn -200))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn game-preload [{:keys [game]}]
  (load-image game "background-tiles" "images/background-tiles.png")
  (load-image game "floor" "images/floor.png")
  (load-image game "finn" "images/finn.png")
  (load-image game "lever_base" "images/lever_base.png")
  (load-image game "lever" "images/lever.png"))

(defn game-create [{:keys [game]}]
  (doto game
    (start-physics-system!)
    (draw-background))

  (set! (.. game -world -enableBody)  true)

  (swap! db #(-> %
                 (assoc :cursor (create-cursor-keys game)
                        :space-key (keyboard-add-key game js/Phaser.Keyboard.SPACEBAR))

                 draw-floors
                 draw-levers
                 (assoc :finn (create-finn game)))))

(defn game-update [{:keys [game]}]

  (let [{:keys [finn groups cursor space-key]} @db
        {:keys [left right up]} cursor
        {floor-group :floor} groups]
    (physics-collide game floor-group finn)

    (cond
      (key-down? left)  (finn-go-left finn)
      (key-down? right) (finn-go-right finn)
      :else             (set-velocity-x finn 0))

    (when (and (key-down? up) (.. finn -body -touching -down))
      (set-velocity-y finn -400))

    (when  (key-down? space-key)
      )
    ))

(defn restart-game [{:keys [game]}]
  (start-game-state game "main"))

(add-game-state (:game @db) "main" {:preload #(game-preload @db)
                                    :create  #(game-create @db)
                                    :update  #(game-update @db)})

(restart-game @db)

(set! (.-z (:finn @db)) 100)
