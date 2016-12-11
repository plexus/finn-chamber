(ns finn-chamber.core
  (:require [cljsjs.phaser]
            [finn-chamber.math :refer [abs]]
            [finn-chamber.wrapper :refer [load-image
                                          load-spritesheet
                                          add-tile-sprite
                                          add-sprite
                                          add-game-state
                                          add-animation
                                          play-animation
                                          stop-animations
                                          start-game-state
                                          start-physics-system!
                                          create-group
                                          set-gravity
                                          set-immutable
                                          set-velocity-x
                                          set-velocity-y
                                          set-anchor
                                          set-scale
                                          set-angle
                                          physics-collide
                                          physics-overlap
                                          create-cursor-keys
                                          keyboard-add-key
                                          add-tween
                                          on-key-down
                                          on-key-up
                                          key-down?]]
            [finn-chamber.levels :as levels]))


(enable-console-print!)

(defonce db (atom {:level levels/level-1
                   :floor-positions [2 9 16 23 30 37 44]
                   :game (js/Phaser.Game. 1280 720)}))

(defn dispatch! [f & args]
  (apply swap! db f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-background [game]
  (add-tile-sprite game 0 32 1280 720 "background-tiles"))

(defn draw-floor-tile [game group x y]
  (let [floor (add-sprite game (* x 16) (* y 16) "wall")]
    (.add group floor)
    (set-immutable floor true)
    floor))

(defn draw-floors [{:keys [game level floor-positions] :as db}]
  (let [spec (:floors level)
        group (create-group game)]
    (doall (map (fn [y tiles]
                  (doall (map-indexed (fn [x yes-no]
                                        (when yes-no (draw-floor-tile game group x y)))
                                      tiles))
                  ) floor-positions spec))
    (assoc-in db [:groups :floor] group)))

(defn item-pos [x]
  (* x 16))

(defn item-floor-pos [floor-positions y]
  (* (nth floor-positions (inc y)) 16))

(defn create-lever-sprite [game floor-positions x y angle]
  (doto (add-sprite game (item-pos x) (item-floor-pos floor-positions y) "lever")
    (set-angle angle)
    (set-scale 1.2)
    (set-anchor 0.5 1)))

(defn create-lever-base-sprite [game floor-positions x y]
  (doto (add-sprite game (item-pos x) (item-floor-pos floor-positions y) "lever_base")
    (set-scale 0.8)
    (set-anchor 0.5 1)))

(defn draw-levers [{{:keys [levers]} :level :keys [game floor-positions] :as db}]
  (let [group (create-group game)]
    (-> (reduce (fn [db lever-id]
                  (let [{:keys [x y]} (get levers lever-id)
                        lever (create-lever-sprite game floor-positions x y -45)
                        lever-base (create-lever-base-sprite game floor-positions x y)]
                    (.add group lever)
                    (.add group lever-base)
                    (-> db
                        (assoc-in [:level :levers lever-id :lever] lever)
                        (assoc-in [:level :levers lever-id :lever-base] lever-base)))
                  ) db (keys levers))
        (assoc-in [:groups :levers] group))))

(defn draw-wall [game floor-positions x y]
  (let [start (nth floor-positions y)
        end (nth floor-positions (inc y))]
    (for [row (range (inc start) end)]
      (doto (add-sprite game (item-pos x) (item-pos row) "wall")
        (set-immutable true)))))

(defn draw-walls [{:keys [game floor-positions groups level] :as db}]
  (let [{:keys [walls]} level
        group (create-group game)]
    (doseq [[x y] walls]
      (run! #(.add group %) (draw-wall game floor-positions x y)))
    (assoc-in db [:groups :walls] group)))

(defn create-finn [game]
  (let [finn (add-sprite game 900 (+ 16 48) "finn")]
    (.setTo (.-scale finn) 1.5)
    (set-anchor finn 0.5 0.5)
    (set-gravity finn 600)
    (add-animation finn "walk")
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

(defn pull-lever [{:keys [level finn game] :as db}]
  (let [{:keys [levers]} level]
    (doseq [[_ {:keys [lever lever-base]}] levers]
      (when (or (physics-overlap game finn lever)
                (physics-overlap game finn lever-base))
        (add-tween game lever {:angle (* -1 (.-angle lever))} 300
                   #_js/Phaser.Easing.Circular.Out
                   js/Phaser.Easing.Bounce.Out
                   ))))
  db)

(defn setup-handlers [{:keys [space-key cursor-keys finn] :as db}]
  (let [{:keys [left right]} cursor-keys]
    (on-key-down space-key #(dispatch! pull-lever))

    (on-key-down left #(play-animation finn "walk" 10 true))
    (on-key-down right #(play-animation finn "walk" 10 true)))
  db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn game-preload [{:keys [game] :as db}]
  (load-image game "background-tiles" "images/background-tiles.png")
  (load-image game "floor" "images/floor.png")
  (load-spritesheet game "finn" "images/finn.png" 32 48 2)
  (load-image game "lever_base" "images/lever_base.png")
  (load-image game "lever" "images/lever.png")
  (load-image game "wall" "images/wall.png")
  db)

(defn game-create [{:keys [game] :as db}]
  (doto game
    (start-physics-system!)
    (draw-background))

  (set! (.. game -world -enableBody)  true)

  (-> db
      (assoc :cursor-keys (create-cursor-keys game))
      (assoc :space-key (keyboard-add-key game js/Phaser.Keyboard.SPACEBAR))
      draw-floors
      draw-levers
      draw-walls
      (assoc :finn (create-finn game))
      setup-handlers))

(defn game-update [{:keys [finn groups cursor-keys space-key level game] :as db}]

  (let [{:keys [left right up]} cursor-keys
        {:keys [levers]} level
        {floor-group :floor lever-group :levers wall-group :walls} groups]
    (physics-collide game floor-group finn)
    (physics-collide game wall-group finn)

    (cond
      (key-down? left)  (finn-go-left finn)
      (key-down? right) (finn-go-right finn)
      :else             (do (set-velocity-x finn 0)
                            (stop-animations finn)))

    (when (and (key-down? up) (.. finn -body -touching -down))
      (set-velocity-y finn -400))

    db))

(defn restart-game [{:keys [game]}]
  )


(restart-game @db)


(defonce started
  (do
    (add-game-state (:game @db) "main" {:preload #(swap! db game-preload)
                                        :create  #(swap! db game-create)
                                        :update  #(swap! db game-update)})
    (start-game-state (:game @db) "main")))
