(ns squares-lgj.render
  (:require [quil.core :as q :include-macros true]))

(def max-width (max 600 js/window.innerWidth))
(def max-height (max 600 js/window.innerHeight))
(def base-unit (/ (min max-height max-width) 100))
(def circle-radius (* 5 base-unit))
(def background [247,255,247])
(def player-color [41,47,54])
(def candy-color [78,205,196])
(def enemy-color [152,12,232])

(defn circle [circle]
  (q/ellipse (:x (:pos circle))
             (:y (:pos circle))
             circle-radius
             circle-radius))

(defn player [player]
  (apply q/fill player-color)
  (circle player))

(defn candy [candy]
  (apply q/fill candy-color)
  (circle candy))

(defn enemy [enemy]
  (apply q/fill enemy-color)
  (circle enemy))

(defn score [score]
  (apply q/fill candy-color)
  (q/text-size 20)
  (q/text-num score (- (/ max-width 2) 25) 25))

(defn start [state]
  (apply q/fill enemy-color)
  (q/text-size 45)
  (q/text "Circles" 140 200)
  (apply q/fill candy-color)
  (q/text-size 25)
  (q/text "Click to start" 255 355)
  (player {:pos {:x 215 :y 330}})
  (enemy {:pos {:x 320 :y 170}})
  (candy {:pos {:x 25 :y 25}}))

(defn playing [state]
  (player (:player state))
  (candy (:candy state))
  (score (+ (:score state)
            (count (:enemies state))))
  (doseq [e (:enemies state)] (enemy e)))

(defn game-over [state]
  (player (:player state))
  (candy (:candy state))
  (doseq [e (:enemies state)] (enemy e))
  (apply q/fill candy-color)
  (let [score (:score state)
        max-score (max (:max-score state) score)]
    (q/text-size 40)
    (q/text "Game over!" 155 255)
    (q/text-size 20)
    (q/text "Score" 205 295)
    (q/text-num (:score state) 285 295)
    (q/text "Max" 205 320)
    (q/text-num max-score 285 320)
    (q/text "Click to restart" 205 355)))

(defn state [state]
  (apply q/background background)
  (condp = (:mode state)
    :start (start state)
    :playing   (playing state)
    :game-over (game-over state)
    :default state))
