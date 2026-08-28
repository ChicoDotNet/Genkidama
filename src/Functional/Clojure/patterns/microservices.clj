(defn microservices-pattern [] (let [stock (atom 7) reserve #(if (> % @stock) false (do (swap! stock - %) true))] (and (= "confirmed" (if (reserve 2) "confirmed" "rejected"))(= @stock 5))))
