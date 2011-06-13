(defn fn-name [fn]
  (let [repr (str fn)]
    (nth (nth (re-seq #"\$([a-zA-Z0-9_]*)@" repr) 0) 1)))

(defn test-fn [fn & tests]
  (println "Testing" (fn-name fn))
  (defn test-body 
      ([passed total] 
        (println "Passed" passed "of" total)
        (when (= passed total)
          (println "Everything passed! Hooray!\n")))
      ([passed total & more]
        (let [passed? (= (apply fn (first more)) (second more))
              new-passed (if passed? (+ passed 1) passed)]
          (when-not passed? (print "-"))
          (apply test-body (list* new-passed (+ total 1) (next (next more)))))))
       
  (apply test-body (list* 0 0 tests)))
