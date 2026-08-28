(defn active-object-pattern [] (let [v (atom 0) q [#(swap! v + 3) #(swap! v * 4)] before @v] (doseq [c q](c))(and (= before 0)(= @v 12))))
