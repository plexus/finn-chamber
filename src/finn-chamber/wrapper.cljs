(ns finn-chamber.wrapper
  (:require [cljsjs.phaser]))


(defn ->js-obj
  "Flat conversion from map with keyword keys to js-obj"
  [m]
  (apply js-obj (mapcat (juxt (comp name first) last) m)))

(defn load-image [game name image]
  (.. game -load (image name image)))

(defn add-tile-sprite [game x y w h name]
  (.. game -add (tileSprite x y w h name)))

(defn add-sprite [game x y name]
  (.. game -add (sprite x y name)))

(defn add-game-state [game name state]
  (.. game -state (add name (->js-obj state))))

(defn start-game-state [game name]
  (.. game -state (start name)))

(defn start-physics-system! [game]
  (.. game -physics (startSystem js/Phaser.Physics.ARCADE)))

(defn create-group [game]
  (.. game -add group))

(defn set-gravity [sprite amount]
  (set! (.. sprite -body -gravity -y) amount))

(defn set-velocity-x [sprite amount]
  (set! (.. sprite -body -velocity -x) amount))

(defn set-velocity-y [sprite amount]
  (set! (.. sprite -body -velocity -y) amount))

(defn physics-collide [game sprite1 sprite2]
  (.. game -physics -arcade (collide sprite1 sprite2)))

(defn set-immutable [sprite val]
  (set! (.. sprite -body -immovable) val))

(defn create-cursor-keys [game]
  (.. game -input -keyboard createCursorKeys))
