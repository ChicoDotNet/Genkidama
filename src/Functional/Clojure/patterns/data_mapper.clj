(defn data-mapper-pattern [] (= {:key "person:8" :name "Grace"} (let[p {:id 8 :name "Grace"}]{:key (str "person:" (:id p)):name (:name p)})))
