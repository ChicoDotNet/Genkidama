(defn distributed-proxy-pattern [] (= 7 (((fn [remote] (fn [sku] (remote sku))) (fn [sku] (if (= sku "sku-1") 7 0))) "sku-1")))
