;(defn test-fn [& f])

(ns game.test.core
  (:require [src.game.test])
)

;(set! *warn-on-reflection* true)

(import java.awt.Dimension)
(import java.awt.event.KeyListener)
(import javax.swing.JPanel)
(import javax.swing.JFrame)

(def *DEBUG* true)

(def *width* 200)
(def *height* 200)
(def *delay* 100)
(def *tile-width* 16)
(def *map-width* 16)

(def x (atom 0))
(def y (atom 0))

(def running (atom true))

(def keys-down (map (fn [x] (atom 0)) (range 255)))

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

(defmacro debug-do [& body]
  (when *DEBUG*
    `(do ~@body)))

(defmacro when [test & body] 
  (list 'if test `(do ~@body) 0))

(defn get-keys-down [keys-down]
  ;Map all 1s to their position in the list, then filter out all 0s. Yay functional!
  (filter (fn [x] (not (= 0 x))) (for [x (range 255)] (if (= @(nth keys-down x) 0) 0 x))))

(def panel
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. 100 100))
    (keyPressed [e]
      (let [key-code (.getKeyCode e)]
        (reset! (nth keys-down key-code) 1)))
    (keyReleased [e] 
      (let [key-code (.getKeyCode e)]
        (reset! (nth keys-down key-code) 0)))
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

(test-fn get-tile-data 
         '(1 1) \0
         '(1 2) \1)

(defn rel-to-abs [x y]
  "Take (1 2) and returns the absolute position of that tile." 
  [(* x *tile-width*)
   (+ 30 (* y *tile-width*))])

(test-fn rel-to-abs
         '(0 0) '(0 30))

(defn in? [list item]
  (some #(= item %) list))

(test-fn in?
         '((1 2 3 4 6) 1) true
         '((1 2 3 4 6) 2) true
         '((1 2 3 4 6) 5) nil
         )

(defn +list [a b]
  (for [x (range (count a))]
    (+ (nth a x) (nth b x))))

(test-fn +list
         '((1 2 3) (2 3 4)) '(3 5 7)
         '((1) (5)) '(6)
         )


"87 W
 65 A
 83 S
 68 D"
(defn get-delta [keys-down]
  (let [W 87
        A 65
        S 83
        D 68
        dy (+ (when (in? keys-down W) -1)
              (when (in? keys-down S)  1))
        dx (+ (when (in? keys-down D)  1)
              (when (in? keys-down A) -1))]
    (list dx dy)))

(defn valid-position? [pos]
  (let [[pos-x pos-y] pos]
    (not (= (get-tile-data pos-x pos-y) \1))))

;TODO: Actual implementation of this fn.
(defn set-whats-necessary [pos]
  (let [[new-x new-y] pos]
    (reset! x new-x)
    (reset! y new-y)))

(defn game-step [keys-down]
  (let [delta (get-delta keys-down)
        new-pos (+list delta (list @x @y))]
    (when (valid-position? new-pos)
      (set-whats-necessary new-pos))))

;
; Graphics
;


(defn draw-tile [gfx x y type]
  "Draws an individual tile."
  (let [[tile-x tile-y] (rel-to-abs x y)]
    (.setColor gfx type)
    (.fillRect gfx tile-x tile-y *tile-width* *tile-width*)))

(defn draw-tiles [gfx]
  "Draw all tiles."
  (doseq [x (range *map-width*)
          y (range *map-width*)]
    (draw-tile gfx x y (get-tile-type (get-tile-data x y)))))

(defn draw-characters [gfx window]
  "Draw all characters."
  (doseq [key (keys *characters*)]
    (draw-tile gfx @x @y (get-tile-type \c))))

(defn draw-state [gfx window]
  "The top level drawing function."
  (let [image (.createImage window *width* *height*)
        off-gfx (.createGraphics image)]
    (draw-tiles off-gfx)
    (draw-characters off-gfx window)
    (.drawImage gfx image 0 0 window)))

(defn make-frame [w h]
  (def frame (javax.swing.JFrame.))

  (.setVisible frame true)
  (.setSize frame w h)
  (.add frame panel)
  (.pack frame)
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
  frame)

(defn game-loop [frame]
  (let [gfx (.getGraphics frame)]
    (Thread/sleep *delay*)
    (game-step (get-keys-down keys-down))
    (draw-state gfx frame)
      (recur frame)))

(defn run-tests [x]
  (println "Testing..."))

(defn main []
  ;Run game-loop in a separate thread so that repl works.
  (do (game-loop (make-frame *width* *height*))))


(main)
