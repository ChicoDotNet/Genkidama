(defn enterprise-adapter-pattern [] (= {:id 17 :amount 12.5} (let [l {:code 17 :cents 1250}] {:id (:code l):amount (/ (:cents l) 100.0)})))
