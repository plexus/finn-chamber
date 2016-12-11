(ns finn-chamber.core
  (:require [cljsjs.phaser]
            [finn-chamber.math :refer [abs]]
            [finn-chamber.util :refer [map-vals]]
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
                                          set-alpha
                                          sprite-scale-x
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

(extend-type js/Phaser.Group
  ISeqable
  (-seq [group]
    (let [children (.-children group)]
      (map #(aget children %) (range (.-length children)))))

    )

;;   ILookup
;;   (-lookup
;;     ([o k] (-lookup o k nil))
;;     ([o k nf] (phaser-get o k get-properties nf))




(defonce db (atom {:level levels/level-1
                   :floor-positions [2 9 16 23 30 37 44]
                   :game (js/Phaser.Game. 1280 720)}))

(defn dispatch! [f & args]
  (apply swap! db f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grid-pos [x]
  (* x 16))

(defn item-floor-pos [floor-positions y]
  (* (nth floor-positions y) 16))


(defn state-angle [state]
  (case state :left -45 :right 45))

(def state-flip {:left :right
                 :right :left})

(defn draw-background [game]
  (add-tile-sprite game 0 32 1280 720 "background-tiles"))

(defn draw-barrier-sprite
  ([game x y]
   (draw-barrier-sprite x y true))
  ([game x y visible?]
   (doto (add-sprite game x y "wall")
     (set-immutable true)
     (set-alpha (if visible? 1 0)))))


(defn draw-barrier-tiles [game y-mapping spec visible?]
  (->> spec
       (mapcat (fn [{:keys [x y group] :as tile}]
                (let [x (grid-pos x)
                      ys (y-mapping y)]
                  (for [y ys]
                    (assoc tile :sprite (draw-barrier-sprite game x y (visible? group)))))))
       doall))

(defn floor-y-mapping [floor-positions]
  (fn [y]
    #{(item-floor-pos floor-positions y)}))

(defn wall-y-mapping [floor-positions]
  (fn [y]
    (let [start (nth floor-positions y)
          end (nth floor-positions (inc y))]
      (map grid-pos (range (inc start) end)))))

(defn draw-barriers [{:keys [game level] :as db} type y-mapping]
  (let [spec (get level type)
        visible? (:visible level)
        tiles (draw-barrier-tiles game y-mapping spec visible?)
        groups (apply array (group-by :group tiles))]
    (-> db
        (assoc type tiles)
        (assoc-in [:groups type] (create-group game (remove nil? (map :sprite tiles))))
        (update :segments #(merge-with into % groups)))))

(defn draw-floors [{:keys [game level floor-positions] :as db}]
  (draw-barriers db :floors (floor-y-mapping floor-positions)))

(defn draw-walls [{:keys [game floor-positions level] :as db}]
  (draw-barriers db :walls (wall-y-mapping floor-positions)))

(defn create-lever-sprite [game floor-positions x y angle]
  (doto (add-sprite game (grid-pos x) (item-floor-pos floor-positions (inc y)) "lever")
    (set-angle angle)
    (set-scale 1.2)
    (set-anchor 0.5 1)))

(defn create-lever-base-sprite [game floor-positions x y]
  (doto (add-sprite game (grid-pos x) (item-floor-pos floor-positions (inc y)) "lever_base")
    (set-scale 0.8)
    (set-anchor 0.5 1)))

(defn draw-levers [{{:keys [levers]} :level :keys [game floor-positions] :as db}]
  (let [group (create-group game)]
    (-> (reduce (fn [db lever-id]
                  (let [{:keys [x y state]} (get levers lever-id)
                        lever (create-lever-sprite game floor-positions x y (state-angle state))
                        lever-base (create-lever-base-sprite game floor-positions x y)]
                    (.add group lever)
                    (.add group lever-base)
                    (-> db
                        (assoc-in [:level :levers lever-id :sprite] lever)
                        (assoc-in [:level :levers lever-id :base-sprite] lever-base)))
                  ) db (keys levers))
        (assoc-in [:groups :levers] group))))

(defn create-finn [game]
  (let [finn (add-sprite game 900 (+ 16 48) "finn")]
    (.setTo (.-scale finn) 1.5)
    (set-anchor finn 0.5 0.5)
    (set-gravity finn 600)
    (add-animation finn "walk")
    finn))

(defn finn-go-left [finn]
  (sprite-scale-x finn (abs (.. finn -scale -x)))
  (set-velocity-x finn -200))

(defn finn-go-right [finn]
  (sprite-scale-x finn (* -1 (abs (.. finn -scale -x))))
  (set-velocity-x finn 200))

(defn finn-jump [finn]
  (set-velocity-y finn -200))

(defn hide-segment [{:keys [game] :as db} name]
  (let [sprites (get-in db [:segments name])]
    (run!
     #(add-tween game (:sprite %) {:alpha 0} 700 js/Phaser.Easing.Cubic.Out)
     sprites))
  db)

(defn show-segment [{:keys [game] :as db} name]
  (let [sprites (get-in db [:segments name])]
    (run!
     #(add-tween game (:sprite %) {:alpha 1} 700 js/Phaser.Easing.Cubic.Out)
     sprites))
  db)

(defn show-hide-segments [db [on-left on-right] state]
  (let [hide (case state :left on-left :right on-right)
        show (case state :left on-right :right on-left)]
    (run! #(show-segment db %) show)
    (run! #(hide-segment db %) hide)
    (-> db
        (update-in [:level :visible] into show)
        (update-in [:level :visible] #(into #{} (remove hide %))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn finn-over-lever? [game finn lever]
  (let [{:keys [sprite base-sprite]} lever]
    (if (or (physics-overlap game finn sprite)
            (physics-overlap game finn base-sprite))
      lever)))

(defn animate-lever [game sprite state]
  (add-tween game
             sprite
             {:angle (-> state state-flip state-angle)} 300
             js/Phaser.Easing.Bounce.Out))

(defn pull-lever [{:keys [game] :as db} lever]
  (let [{:keys [sprite base-sprite state hides name]} lever]
    (animate-lever game sprite state)
    (-> db
        (show-hide-segments hides state)
        (update-in [:level :levers name :state] state-flip))))

(defn handle-space [{{:keys [levers]} :level :keys [finn game] :as db}]
  (if-let [lever (some #(finn-over-lever? game finn %) (vals levers))]
    (pull-lever db lever)
    db))

(defn setup-handlers [{:keys [space-key cursor-keys finn] :as db}]
  (let [{:keys [left right]} cursor-keys]
    (on-key-down space-key #(dispatch! handle-space))

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

  (set! (.. game -scale -scaleMode) js/Phaser.ScaleManager.EXACT_FIT)
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
        {floor-group :floors lever-group :levers wall-group :walls} groups]
    (physics-collide game floor-group finn)
    (physics-collide game wall-group finn)

    (doseq [sprite floor-group]
      (if (= 0 (.-alpha sprite))
        (set! (.-exists sprite) false)
        (set! (.-exists sprite) true)))

    (cond
      (key-down? left)  (finn-go-left finn)
      (key-down? right) (finn-go-right finn)
      :else             (do (set-velocity-x finn 0)
                            (stop-animations finn)))

    (when (and (key-down? up) (.. finn -body -touching -down))
      (set-velocity-y finn -400))

    db))


(defonce started
  (do
    (add-game-state (:game @db) "main" {:preload #(swap! db game-preload)
                                        :create  #(swap! db game-create)
                                        :update  #(swap! db game-update)})
    (start-game-state (:game @db) "main")))
