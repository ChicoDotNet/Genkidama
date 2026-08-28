(defn message-bus-pattern [] (= ["audit:order-created:42" "billing:order-created:42"] (mapv #(% "order-created" 42) [(fn[t i](str "audit:" t ":" i))(fn[t i](str "billing:" t ":" i))])))
