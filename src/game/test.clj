;(ns game-tests
;  (:require clojure.contrib.str-utils))

(defn fn-name [fn]
  (let [repr (str fn)]
;    (clojure.contrib.str-utils/replace-first-str "_QMARK_" "?"
      (nth (nth (re-seq #"\$([a-zA-Z0-9_]*)@" repr) 0) 1))) ;)

(defn test-fn [fn & tests]
  (println "Testing" (fn-name fn))
  (defn test-body 
    ; run a test
    ([passed total & more]
      (let [result (apply fn (first more))
            expected (second more)
            passed? (= result expected)
            new-passed (if passed? (+ passed 1) passed)]
        (when-not passed? (println "Got:" result ", expected:" expected))
        (apply test-body (list* new-passed (+ total 1) (next (next more))))))

    ; done; print out statistics
    ([passed total] 
      (println "Passed" passed "of" total)
      (when (= passed total)
        (println "Everything passed! Hooray!\n"))))

       
  (apply test-body (list* 0 0 tests)))
