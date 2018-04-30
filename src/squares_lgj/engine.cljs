(ns squares-lgj.engine
  (:require [quil.core :as q :include-macros true]
            [squares-lgj.io :as io]
            [squares-lgj.render :as render]))

(def min-vel (* 1 render/base-unit))
(def max-vel (* 2 render/base-unit))

(def max-x (- render/max-width render/circle-radius))
(def max-y (- render/max-height render/circle-radius))

(defn gen-with-constraint [generator constraint]
  (first (drop-while #(not (constraint %))
                     (repeatedly generator))))

(defn rand-pos
  ([] (rand-pos (constantly true) (constantly true)))
  ([x-pred y-pred]
   {:x (gen-with-constraint #(rand-int max-x) x-pred)
    :y (gen-with-constraint #(rand-int max-y) y-pred)}))

(defn new-enemy [player]
  (let [{player-x :x  player-y :y} (:pos player)
        buffer-zone 30]
    {:pos (rand-pos #(not (< (- player-x buffer-zone) % (+ player-x buffer-zone)))
                    #(not (< (- player-y buffer-zone) % (+ player-y buffer-zone))))
     :vel {:x 0 :y 0}}))

(defn spawn-candy []
  {:pos (rand-pos)})

(defn square [x] (* x x))

(defn collision? [a b]
  (let [xa (:x (:pos a))
        ya (:y (:pos a))
        xb (:x (:pos b))
        yb (:y (:pos b))]
    (< (+ (square (- xa xb)) (square (- ya yb)))
       (square render/circle-radius))))

(def mag-damper (/ 1 2))
(def bounce-damper (/ 1 3))
(def dist-buffer 0.001)
(def gravity 3)

(defn unit-vector
  "returns x and y components of a vector from enemy to player."
  [player enemy]
  (let [{px :x py :y} (:pos player)
        {ex :x ey :y} (:pos enemy)
        [x y] [(- px ex) (- py ey)]
        mag-fn (fn [x y] (Math.sqrt (+ (square x) (square y))))
        dist (Math.sqrt (+ (square x) (square y)))
        ;; percentage of board size
        mag (mag-fn x y)
        [x y] [(/ x mag) (/ y mag)]
        grav (* (/ 1 (Math.sqrt dist)) gravity)]
    {:x-player-dir (* x grav)
     :y-player-dir (* y grav)}))

(defn clamp [lil big n] (max lil (min big n)))

(defn move-enemy [player enemy]
  (let [{:keys [x-player-dir y-player-dir]} (unit-vector player enemy)
        vx (+ (:x (:vel enemy)) x-player-dir)
        vy (+ (:y (:vel enemy)) y-player-dir)
        new-x (+ (:x (:pos enemy)) vx)
        new-y (+ (:y (:pos enemy)) vy)
        new-vel (cond
                  (not (< 0 new-x max-x))
                  {:x (* bounce-damper (- vx)) :y vy}
                  (not (< 0 new-y max-y))
                  {:x vx :y (* bounce-damper (- vy))}
                  :else
                  {:x vx :y vy})]
    {:pos {:x (clamp 0 max-x new-x)
           :y (clamp 0 max-y new-y)}
     :vel new-vel}))

(defn spawn-enemy
  ([player] (spawn-enemy player :candy []))
  ([player event enemies]
   (condp = event
     :candy (conj enemies (new-enemy player))
     enemies)))

(defn check-collisions [state]
  (cond
    (collision? (:player state) (:candy state)) :candy
    ;;(reduce #(or (collision? (:candy state) %2) %1) false (:enemies state)) :candy
    (reduce #(or (collision? (:player state) %2) %1) false (:enemies state)) :enemy
    :default :nothing))

(defn play [state]
  (let [event  (check-collisions state)
        player (io/get-player)]
    (-> state
        #_(update-in [:enemies] #(if (= % [])
                                   [(spawn-enemy player)]
                                   %))
        (assoc-in [:candy] (condp = event
                             :candy (spawn-candy)
                             (:candy state)))
        (update-in [:mode] (condp = event
                             :enemy (fn [a] :game-over)
                             identity))
        (update-in [:score] (condp = event
                              :candy inc
                              identity))
        (assoc-in [:player] player)
        (update-in [:enemies] #(map (partial move-enemy player) %))
        (update-in [:enemies] #(spawn-enemy player event %)))))

(defn update-state [state]
  (condp = (:mode state)
    :playing (play state)
    state))
