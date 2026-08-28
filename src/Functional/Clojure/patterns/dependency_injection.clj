(defn dependency-injection-pattern [] (= "at:10:00" ((fn[clock](str "at:" (clock))) (constantly "10:00"))))
