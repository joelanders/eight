(ns eight.graphics
  (:use seesaw.core
        seesaw.graphics
        seesaw.color))

(defonce f (frame :title "testing"))
(defonce c (canvas :id :canvas :background "#ffffff" :paint nil))

(defonce global-canvas nil) ;; assume we have a canvas

(defonce list-of-pixels (atom (hash-map))) ;; maps [x y] to [r g b]

(defn set-pixel-on-canvas [[x y] [r g b]]
  (swap! list-of-pixels (fn [old-hash-map] (assoc old-hash-map [x y] [r g b])))
  (if (not (nil? global-canvas)) (repaint! global-canvas)))

(defn blank-canvas []
  (swap! list-of-pixels {})
  (if (not (nil? global-canvas)) (repaint! global-canvas)))

(defn initialise-canvas [pixels-to-draw]
  "create the global canvas and show it"
  (let [my-canvas (seesaw.core/canvas)
        frame (seesaw.core/frame :title :paint-board :content my-canvas)]
    (config! my-canvas :paint 
      (fn [c gr]
        (doseq [pixel @pixels-to-draw]
          (let [[x y] (key pixel)
                [r g b] (val pixel)]
            (draw gr (rect (* 5 x) (* 5 y) 5 5)
                  (style :foreground (color r g b 0)
                         :background (color r g b 255)))))))
    (pack! frame)
    (show! frame)
    (def global-canvas my-canvas)))

(if (nil? global-canvas) ;; only do this the first time
  (initialise-canvas list-of-pixels))

(defn draw-square [square [xi yi]]
  (doseq [index (for [x (range 0 (count square))
                      y (range 0 (count (first square)))]
                  [x y])]
      (set-pixel-on-canvas [(+ xi (first index)) (+ yi (second index))]
                           (into [] (repeat 3 (* 255 (/ (+ 1 (get-in square index)) 2)))))))
