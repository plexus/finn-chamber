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
                                          physics-collide
                                          create-cursor-keys]]
            [finn-chamber.levels :as levels]))


(enable-console-print!)

(defonce game (js/Phaser.Game. 1280 720))

(def db (atom {}))

(def floor-positions [0 7 14 21 29 37 44])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-background [game]
  (add-tile-sprite game 0 0 1280 720 "background-tiles"))

(defn draw-floor-tile [game group x y]
  (let [floor (add-sprite game (* x 16) (* y 16) "floor")]
    (.add group floor)
    (set-immutable floor true)
    floor))

(defn draw-floors [game group spec]
  (doall (map (fn [y tiles]
                (doall (map-indexed (fn [x yes-no]
                                      (when yes-no (draw-floor-tile game group x y)))
                                    tiles))
                ) floor-positions spec)))

(defn create-finn [game]
  (let [finn (add-sprite game 900 (+ 16 48) "finn")]
    (.setTo (.-scale finn) 1.5)
    (.setTo (.-anchor finn) 0.5 0.5)
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

(defn game-preload []
  (load-image game "background-tiles" "images/background-tiles.png")
  (load-image game "floor" "images/floor.png")
  (load-image game "finn" "images/finn.png"))

(defn game-create []
  (doto game
    (start-physics-system!)
    (draw-background))

  (set! (.. game -world -enableBody)  true)


  (let [floors (create-group game)
        finn (create-finn game)]

    (draw-floors game floors levels/floors)

    (set-gravity finn 600)
    (swap! db assoc
           :finn finn
           :floors floors
           :cursor (create-cursor-keys game)

           )))

(defn game-update [this]

  (let [{:keys [finn floors cursor]} @db]
    (physics-collide game floors finn)

    (cond
      (.. cursor -left -isDown)  (finn-go-left finn)
      (.. cursor -right -isDown) (finn-go-right finn)
      :else                      (set-velocity-x finn 0))

    (when (and (.. cursor -up -isDown) (.. finn -body -touching -down))
      (set-velocity-y finn -600))
    ))

(defn restart-game []
  (start-game-state game "main"))

(add-game-state game "main" {:preload game-preload
                             :create  game-create
                             :update  (fn [] (this-as self (game-update self)))})

(restart-game)

(.-body (create-finn game))

(set! js/window.db @db)

(.-velocity (.-body (:finn @db)))
