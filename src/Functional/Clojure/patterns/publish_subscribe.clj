(defn publish-subscribe-pattern [] (= ["warehouse:51" "analytics:51"] (mapv #(% 51) [(fn[i](str "warehouse:" i))(fn[i](str "analytics:" i))])))
