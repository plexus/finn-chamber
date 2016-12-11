(ns finn-chamber.core
  (:require [clojure.data :refer [diff]]
            [finn-chamber.levels :as levels]
            [finn-chamber.math :refer [abs]]
            [finn-chamber.util :refer [deep-merge map-vals]]
            [finn-chamber.wrapper :refer [add-animation
                                          add-game-state
                                          add-sprite
                                          add-tile-sprite
                                          add-tween
                                          create-cursor-keys
                                          create-group
                                          key-down?
                                          keyboard-add-key
                                          load-image
                                          load-spritesheet
                                          on-key-down
                                          physics-collide
                                          physics-overlap
                                          play-animation
                                          set-alpha
                                          set-anchor
                                          set-angle
                                          set-gravity
                                          set-immutable
                                          set-scale
                                          set-velocity-x
                                          set-velocity-y
                                          sprite-scale-x
                                          start-game-state
                                          start-physics-system!
                                          stop-animations]]))

(enable-console-print!)

(extend-type js/Phaser.Group
  ISeqable
  (-seq [group]
    (let [children (.-children group)]
      (map #(aget children %) (range (.-length children))))))

;;   ILookup
;;   (-lookup
;;     ([o k] (-lookup o k nil))
;;     ([o k nf] (phaser-get o k get-properties nf))



(defonce initial-state
  {:level 0
   :floor-positions [2 9 16 23 30 37 44]
   :game (js/Phaser.Game. 1280 720)
   :history '()})

(defonce db (atom initial-state))

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

(defn draw-barriers [{:keys [game visible] :as db} type y-mapping]
  (let [spec (get db type)
        tiles (draw-barrier-tiles game y-mapping spec visible)
        groups (apply array (group-by :group tiles))]
    (-> db
        (assoc type tiles)
        (assoc-in [:groups type] (create-group game (remove nil? (map :sprite tiles))))
        (update :segments #(merge-with into % groups)))))

(defn draw-floors [{:keys [game floor-positions] :as db}]
  (draw-barriers db :floors (floor-y-mapping floor-positions)))

(defn draw-walls [{:keys [game floor-positions] :as db}]
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

(defn draw-levers [{:keys [game floor-positions levers] :as db}]
  (let [group (create-group game)]
    (-> (reduce (fn [db lever-id]
                  (let [{:keys [x y state]} (get levers lever-id)
                        lever (create-lever-sprite game floor-positions x y (state-angle state))
                        lever-base (create-lever-base-sprite game floor-positions x y)]
                    (.add group lever)
                    (.add group lever-base)
                    (-> db
                        (assoc-in [:levers lever-id :sprite] lever)
                        (assoc-in [:levers lever-id :base-sprite] lever-base)))
                  ) db (keys levers))
        (assoc-in [:groups :levers] group))))

(defn finn-start-pos [{:keys [floor-positions] :as db}]
  (let [{:keys [x y]} (-> db :segments :$ first)]
    [(grid-pos x) (item-floor-pos floor-positions (inc y))]))

(defn create-finn [{:keys [game] :as db}]
  (let [[x y] (finn-start-pos db)]
    (assoc db :finn
           (doto (add-sprite game x y "finn")
             (set-scale 1.5)
             (set-anchor 0.5 1)
             (set-gravity  600)
             (add-animation  "walk")))))

(defn finn-go-left [finn]
  (sprite-scale-x finn (abs (.. finn -scale -x)))
  (set-velocity-x finn -400))

(defn finn-go-right [finn]
  (sprite-scale-x finn (* -1 (abs (.. finn -scale -x))))
  (set-velocity-x finn 400))

(defn finn-jump [finn]
  (set-velocity-y finn -200))

(defn hide-segment [{:keys [game] :as db} name]
  (let [sprites (get-in db [:segments name])]
    (run!
     #(when-not (= (.-alpha %) 0)
        (add-tween game (:sprite %) {:alpha 0} 700 js/Phaser.Easing.Cubic.Out))
     sprites))
  db)

(defn show-segment [{:keys [game] :as db} name]
  (let [sprites (get-in db [:segments name])]
    (run!
     #(when-not (= (.-alpha %) 1)
        (add-tween game (:sprite %) {:alpha 1} 700 js/Phaser.Easing.Cubic.Out))
     sprites))
  db)

(defn update-visible [db [on-left on-right] state]
  (let [hide (case state :left on-left :right on-right)
        show (case state :left on-right :right on-left)]
    (-> db
        (update-in [:visible] into show)
        (update-in [:visible] #(into #{} (remove hide %))))))

(defn show-hide-segments [db]
  (doseq [[name _] (:segments db)]
    (if (-> db :visible name)
      (show-segment db name)
      (hide-segment db name)))
  db)

(defn game-state [db]
  {:levers (-> db :levers)
   :visible (-> db :visible)
   :finn {:x (.-x (:finn db))
          :y (.-y (:finn db))}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn finn-over-lever? [game finn lever]
  (let [{:keys [sprite base-sprite]} lever]
    (if (or (physics-overlap game finn sprite)
            (physics-overlap game finn base-sprite))
      lever)))

(defn animate-lever [game sprite state]
  (add-tween game
             sprite
             {:angle (-> state state-angle)} 300
             js/Phaser.Easing.Bounce.Out))

(defn pull-lever [{:keys [game] :as db} lever]
  (let [{:keys [sprite base-sprite state hides name]} lever]
    (animate-lever game sprite (state-flip state))
    (-> db
        (#(update % :history conj (game-state %)))
        (update-visible hides state)
        show-hide-segments
        (update-in [:levers name :state] state-flip))))

(defn handle-space [{:keys [levers finn game] :as db}]
  (if-let [lever (some #(finn-over-lever? game finn %) (vals levers))]
    (pull-lever db lever)
    db))

(defn restore-game-state [{:keys [finn game] :as db} state]
  (set! (.-x finn) (-> state :finn :x))
  (set! (.-y finn) (-> state :finn :y))

  (doseq [{:keys [name sprite state]} (vals (:levers state))]
    (if-not (= state (get-in db [:levers name :state]))
      (animate-lever game sprite state)))

  (-> db
      (update :levers deep-merge (:levers state))
      (assoc :visible (:visible state))
      show-hide-segments))

(defn handle-undo [{:keys [game finn history] :as db}]
  (if-let [state (first history)]
    (-> db
        (restore-game-state state)
        (update :history pop))
    db))

(defn setup-handlers [{:keys [keys finn] :as _db}]
  (let [{:keys [left right space z]} keys]
    (on-key-down space #(dispatch! handle-space))

    (on-key-down left #(play-animation finn "walk" 10 true))
    (on-key-down right #(play-animation finn "walk" 10 true))

    (on-key-down z #(swap! db handle-undo)))

  _db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn game-preload [{:keys [game] :as db}]
  (load-image game "background-tiles" "images/background-tiles.png")
  (load-image game "floor" "images/floor.png")
  (load-spritesheet game "finn" "images/finn.png" 32 48 2)
  (load-image game "lever_base" "images/lever_base.png")
  (load-image game "lever" "images/lever.png")
  (load-image game "wall" "images/wall.png")
  db)

(defn start-level [db]
  (let [db (-> db
               (merge (levels/levels (:level db)))
               (assoc :frames 0)
               draw-floors
               draw-levers
               draw-walls
               create-finn
               setup-handlers)]
    (set-velocity-x (:finn db) -400)
    db))

(defn next-level! [{:keys [walls floors levers finn] :as db}]
  (let [sprites
        (concat (mapcat #(map :sprite %) [walls floors (vals levers)])
                (map :base-sprite (vals levers))
                [finn])]
    (doseq [sprite sprites]
      (if sprite
        (.destroy sprite)))

    (start-level (update db :level inc))))

(defn game-create [{:keys [game] :as db}]
  (doto game
    (start-physics-system!)
    (draw-background))

  (set! (.. game -scale -scaleMode) js/Phaser.ScaleManager.EXACT_FIT)
  (set! (.. game -world -enableBody)  true)

  (-> db
      (assoc :frames 0)
      (assoc :keys (create-cursor-keys game))
      (assoc-in [:keys :space] (keyboard-add-key game js/Phaser.Keyboard.SPACEBAR))
      (assoc-in [:keys :z] (keyboard-add-key game js/Phaser.Keyboard.Z))
      start-level))

(defn finn-handle-keys [finn keys]
  (let [{:keys [left right up]} keys]
    (cond
      (key-down? left)  (finn-go-left finn)
      (key-down? right) (finn-go-right finn)
      :else             (do (set-velocity-x finn 0)
                            (stop-animations finn)))

    (when (and (key-down? up) (.. finn -body -touching -down))
      (set-velocity-y finn -400))))

(defn increase-frame-count [db]
  (update db :frames inc))

(defn handle-end-of-intro [{:keys [frames] :as db}]
  (if (= frames 20)
    (-> db
        (update :visible conj :$)
        show-hide-segments)
    db))

(defn game-update [{:keys [frames finn groups keys game levers] :as db}]
  (let [{floor-group :floors lever-group :levers wall-group :walls} groups]

    (physics-collide game floor-group finn)
    (physics-collide game wall-group finn)

    (if (< frames 20)
      (finn-go-left finn)
      (finn-handle-keys finn keys))


    (doseq [sprite (concat floor-group wall-group)]
      (if (= 0 (.-alpha sprite))
        (set! (.-exists sprite) false)
        (set! (.-exists sprite) true)))

    (if-not (< 0 (.-x finn) 1280)
      (next-level! db)
      (-> db
          increase-frame-count
          handle-end-of-intro))))

(defn start! []
  (add-game-state (:game @db) "main" {:preload #(swap! db #'game-preload)
                                      :create  #(swap! db #'game-create)
                                      :update  #(swap! db #'game-update)})
  (start-game-state (:game @db) "main"))


(defn on-reload []
  (let [old-db @db
        old-state (game-state old-db)]
    (.destroy (:game @db))

    (let [level (levels/levels (:level old-state))]
      (reset! db (-> old-db
                     (assoc :game (js/Phaser.Game. 1280 720))
                     (assoc :floors (:floors level))
                     (assoc :walls (:walls level))
                     (assoc :levers (:levers level)))))

    (doseq [[id {:keys [state]}] (-> old-db :levers)]
      (swap! db assoc-in [:levers id :state] state))

    (start!)


    (js/setTimeout
     #(let [finn (:finn @db)]
        (set! (.-x finn) (-> old-state :finn :x))
        (set! (.-y finn) (-> old-state :finn :y)))
     100)))
