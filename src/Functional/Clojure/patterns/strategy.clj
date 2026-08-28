(defn strategy-pattern [] (let [price #(%2 %1)] (and (= 100 (price 100 identity)) (= 80 (price 100 #(* % 8/10))))))
