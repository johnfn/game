(defn test-fn [fn-name & tests]
  (println "Testing" fn-name)
  (defn test-body 
      ([passed total] 
        (println "Passed" passed "of" total)
        (when (= passed total)
          (println "Everything passed! Hooray!")))
      ([passed total & more]
        (let [passed? (= (apply fn-name (first more)) (second more))
              new-passed (if passed? (+ passed 1) passed)]
          (println (if passed? "PASSED" "FAILED"))
          (apply test-body (list* new-passed (+ total 1) (next (next more)))))))
       
  (apply test-body (list* 0 0 tests)))
