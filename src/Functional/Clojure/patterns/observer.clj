(defn observer-pattern [] (= ["audit:42" "dashboard:42"] (mapv #(% 42) [(fn [i](str "audit:" i))(fn [i](str "dashboard:" i))])))
