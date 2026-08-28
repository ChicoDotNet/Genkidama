(defn lazy-initialization-pattern [] (let [builds(atom 0) v(delay(do(swap! builds inc)"ready"))] (and (= @v "ready")(= @v "ready")(= @builds 1))))
