(defn repository-pattern [] (= "Grace" (:name (first (filter #(= 2 (:id %)) [{:id 1 :name "Ada"} {:id 2 :name "Grace"}])))))
