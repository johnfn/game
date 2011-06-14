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

(def *speed* 4)
(def *width* 200)
(def *height* 200)
(def *delay* 5)
(def *tile-width* 16)
(def *player-width* 12)
(def *map-width* 30)
(def *abs-map-width* (* *map-width* *tile-width*))

(def x (atom 30))
(def y (atom 30))

(def keys-down (map (fn [x] (atom 0)) (range 255)))

(def simple-map
(str
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000101010101000000000000000"
"000000000000000000000000000000"
"000000001111111111110000000000"
"000000000000000000000000000000"
"000000000000000011111111111000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
"000000000000000000000000000000"
))


(def *characters* {"You" [4 4]})

;TODO: better is dbg such that you just put it in the beginning of any form you want to debug and it works by applying.
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defmacro debug-do [& body]
  (when *DEBUG*
    `(do ~@body)))

(defmacro when [test & body] 
  (list 'if test `(do ~@body) 0))

(defn get-keys-down [keys-down]
  ;Map all 1s to their position in the list, then filter out all 0s. Yay functional!
  (filter (fn [x] (not (= 0 x))) (for [x (range 255)] (if (= @(nth keys-down x) 0) 0 x))))

(defn +list [& lists]
  (let [len (count (nth lists 0))]
    (map 
      (fn [x] ;for each position
        (apply +  ;sum
               (map (fn [list] ; each element of each list
                      (nth list x)) ;that's at that position
                    lists))) 
      (range len))))

;(defn +list [a b]
;  (for [x (range (count a))]
;    (+ (nth a x) (nth b x))))

(test-fn +list
         '((1 2 3) (2 3 4)) '(3 5 7)
         '((1 2) (2 3) (3 4)) '(6 9)
         '((1) (5)) '(6)
         )

(def panel
  (proxy [JPanel KeyListener] []
    (getPreferredSize [] (Dimension. *width* *height*))
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

(defn get-tile-data [x y]
  (nth simple-map (+ (* y *map-width*) x)))

(test-fn get-tile-data 
         '(1 1) \0
         '(1 2) \1)

(defn abs-to-abs [x-pos y-pos]
  "Take an absolute position and put it in the correct place. I really need to think of a better fn name."
  (let [x-offs @x
        y-offs @y]
    (+list
      [(+ (- x-offs) 50) (+ (- y-offs) 50)] ;adjust for moving map
      [0 30] ;adjust for title bar
      [x-pos y-pos])))

(defn rel-to-abs [x-pos y-pos]
  "Take (1 2) and returns the absolute position of that tile." 
  [(* x-pos *tile-width*) (* y-pos *tile-width*)])

(defn abs-to-rel [x y]
  (for [i (list x (+ x *player-width*))
        j (list y (+ y *player-width*))]
    [(int (/ i *tile-width*))
     (int (/ j *tile-width*))]))

(defn rel-to-pos [x y]
  (apply abs-to-abs (rel-to-abs x y)))

(test-fn rel-to-pos
         '(0 0) '(0 30))

(defn in? [list item]
  (some #(= item %) list))

(test-fn in?
         '((1 2 3 4 6) 1) true
         '((1 2 3 4 6) 2) true
         '((1 2 3 4 6) 5) nil
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
        dy (+ (when (in? keys-down W) (- *speed*))
              (when (in? keys-down S)    *speed*))
        dx (+ (when (in? keys-down D)    *speed*)
              (when (in? keys-down A) (- *speed*)))]
    (list dx dy)))

(defn offscreen? [pos]
  (let [x (nth pos 0)
        y (nth pos 1)]
    (or (< x 0)
        (> x *abs-map-width*)
        (< y 0)
        (> y *abs-map-width*))))

(defn valid-position? [pos]
  ;Check all (up to 4) positions for validity.
  (if (offscreen? pos) 
    false
    (reduce (fn [x y] (and x y)) ;Ensure that each of them are valid.
      (let [positions (apply abs-to-rel pos)]
        (map 
           (fn [x] 
             (not (= (get-tile-data (nth x 0) (nth x 1)) \1))) 
           positions)))))

;TODO: Actual implementation of this fn.
(defn add-whats-necessary [pos]
  (let [[new-x new-y] pos]
    (reset! x (+ @x new-x))
    (reset! y (+ @y new-y))))

(defn game-step [keys-down]
  (let [[dx dy] (get-delta keys-down)
        char-pos [@x @y]
        vect-x [dx 0]
        vect-y [0 dy]]
    (when (valid-position? (+list vect-x char-pos))
      (add-whats-necessary vect-x))
    (let [char-pos [@x @y]] ;need to rebind because x may have changed.
      (when (valid-position? (+list vect-y char-pos))
        (add-whats-necessary vect-y)))))

;
; Graphics
;

(defn get-tile-type [tile]
  (let [type (cond
               (= tile \0) `(255 255 255)
               (= tile \1) `(255 0 0)
               (= tile \c) `(0 0 255)
              )]
    (java.awt.Color. (nth type 0) (nth type 1) (nth type 2))))


(defn draw-abs [gfx x y type]
  "Draws an absolutely positioned tile"
  (.setColor gfx type)
  (.fillRect gfx x y *tile-width* *tile-width*))

(defn visible-abs? [x y]
  (and (> x (- *tile-width*))
       (< x *abs-map-width*)
       (> y (- *tile-width*))
       (< y *abs-map-width*)))

(defn draw-tile [gfx x y type]
  "Draws an individual tile."
  (let [[tile-x tile-y] (rel-to-pos x y)]
    (when (visible-abs? tile-x tile-y)
      (draw-abs gfx tile-x tile-y type))))


(defn draw-tiles [gfx]
  "Draw all tiles."
  (doseq [x (range *map-width*)
          y (range *map-width*)]
    (draw-tile gfx x y (get-tile-type (get-tile-data x y)))))

(defn draw-characters [gfx window]
  "Draw all characters."
  (doseq [key (keys *characters*)]
    (let [[x y] (abs-to-abs @x @y)]
      (draw-abs gfx x y (get-tile-type \c)))))

(defn draw-state [gfx window]
  "The top level drawing function."
  (let [image (.createImage window *width* *height*)
        off-gfx (.createGraphics image)]
    (draw-tiles off-gfx)
    (draw-characters off-gfx window)
    (.drawImage gfx image 0 0 window)))

(defn game-loop [frame]
  (let [gfx (.getGraphics frame)]
    (Thread/sleep *delay*)
    (game-step (get-keys-down keys-down))
    (time (draw-state gfx frame))
    (recur frame)))

(defn run-tests [x]
  (println "Testing..."))

(defn make-frame [w h]
  (def frame (javax.swing.JFrame.))

  (.setVisible frame true)
  (.setSize frame w h)
  (.add frame panel)
  (.pack frame)
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
  frame)

(defn main []
  ;Run game-loop in a separate thread so that repl works.
  (do (game-loop (make-frame *width* *height*))))


(main)
