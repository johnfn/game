;(ns game.core
; (:gen-class))

(def state :placing)

(defn my-parseint [i]
  (defn parse-helper [i]
    (if (> (count i) 1)
      (+ (parse-helper (vector (first i)))
         (* 10 (parse-helper (rest i))))
      (condp = (first i)
        \1 1
        \2 2
        \3 3
        \4 4
        \5 5
        \6 6
        \7 7
        \8 8
        \9 9
        \0 0
        )))
  (parse-helper (reverse i)))

(defn arr [sz]
  (defn arr-helper [cur-sz]
    (defn helper [cw]
      (if (== cw 0)
        [""]
        (conj (helper (- cw 1)) "s")))
    (if (== cur-sz 0)
      [""]
      (conj (arr-helper (- cur-sz 1)) (helper sz))))
  (arr-helper sz))

(defn get2d [arr i j]
  (get (get arr i) j))

(defn split-line [line]
  (map my-parseint (seq (.split line " "))))

(def tests
  '(("15" 15)
    ("0" 0)
    ("156" 156)))

(defn do-tests [all-tests fn]
  (defn test-helper [tests-left passed total fn]
    (if (not (= (count tests-left) 0))
      (let [current-test (first tests-left)
            expected (last current-test)
            received (fn (first current-test))]
        (test-helper (rest tests-left)
                     (+ passed
                       (if (= expected received)
                         (do (println "PASSED") 1)
                         (do (println "FAILED. Expected" expected "; received " received) 0)))
                     (+ 1 total)
                     fn))
      ;print statistics
      (do
        (println "Results:")
        (println "Passed:" passed "of" total)
        (if (= passed total)
          (println "Passed everything! Awesome!")))))
  (test-helper all-tests 0 0 fn))

(do-tests tests my-parseint)

(defn -main [& args]
  (defn main-loop [state]
    (defn place-piece [line]
        (let [pieces (split-line line)
              x (first pieces)
              y (last pieces)]
          (println x y)))
    (defn make-move [line]
        (let [pieces (split-line line)]
          (println "moving" line)))
    ((condp = state
        :placing place-piece
        :playing make-move) (str (read-line))))
  (def screen (arr 5))
  (main-loop :placing))


(-main)


