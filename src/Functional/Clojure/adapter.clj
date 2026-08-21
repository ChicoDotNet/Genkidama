(defn legacy-read-fahrenheit [] 86)

(defn adapt-to-celsius [read-fahrenheit]
  (fn [] (quot (* (- (read-fahrenheit) 32) 5) 9)))

(let [read-celsius (adapt-to-celsius legacy-read-fahrenheit)]
  (println (str "legacy=" (legacy-read-fahrenheit) "F"))
  (println (str "adapted=" (read-celsius) "C")))
