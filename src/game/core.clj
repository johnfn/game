(set! *warn-on-reflection* true)

(import java.awt.Dimension)
(import java.awt.event.KeyListener)
(import javax.swing.JPanel)
(import javax.swing.JFrame)

(def *width* 200)
(def *height* 200)
(def *delay* 100)
(def *tile-width* 16)
(def *map-width* 16)

(def x (ref 0))
(def y (ref 0))

(def simple-map
  (str
    "0000000000000000"
    "0000000000000000"
    "0111111111111100"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"
    "0000000000000000"))

(def *characters* {"You" [4 4]})

(def panel
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. 100 100))
    (keyPressed [e]
      (let [keyCode (.getKeyCode e)]
        (println keyCode)))
    (keyReleased [e])
    (keyTyped [e])))

(doto panel
  (.setFocusable true)
  (.addKeyListener panel))


(defn get-tile-type [tile]
  (let [type (cond
               (= tile \0) `(255 255 255)
               (= tile \1) `(255 0 0)
               (= tile \c) `(0 0 255)
              )]
    (java.awt.Color. (nth type 0) (nth type 1) (nth type 2))))

(defn get-tile-data [x y]
  (nth simple-map (+ (* y *map-width*) x)))

(defn rel-to-abs [x y]
  "Take (1 2) and adjust them by *tile-width*. Also moves according to title bar."
  [(* x *tile-width*)
   (+ 30 (* y *tile-width*))])

(defn make-frame [w h]
  (def frame (java.awt.Frame.))

  (.setVisible frame true)
  (.setSize frame w h)
  (.add frame panel)
  (.pack frame)
  ;(.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
  frame)

(defn draw-tile [gfx x y type]
  (let [[tile-x tile-y] (rel-to-abs x y)]
    (.setColor gfx type)
    (.fillRect gfx tile-x tile-y *tile-width* *tile-width*)))

(defn draw-tiles [gfx]
  (doseq [x (range *map-width*)
          y (range *map-width*)]
    (draw-tile gfx x y (get-tile-type (get-tile-data x y)))))

(defn draw-characters [gfx window]
  (doseq [key (keys *characters*)]
    (let [[x y] (get *characters* key)]
      (draw-tile gfx x y (get-tile-type \c)))))

(defn draw-state [gfx window]
  (let [image (.createImage window *width* *height*)
        off-gfx (.createGraphics image)]
    (draw-tiles off-gfx)
    (draw-characters off-gfx window)
    
    (.drawImage gfx image 0 0 window)))

(defn game-loop [frame]
  (let [gfx (.getGraphics frame)]
    (Thread/sleep *delay*)
    (draw-state gfx frame)
    (recur frame)))

(defn main []
  (let [w *width*
        h *height*
        frame (make-frame w h)]
    (game-loop frame)))

(main)
