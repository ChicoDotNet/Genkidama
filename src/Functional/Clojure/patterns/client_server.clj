(defn client-server-pattern [] (= {:status 200 :body "stock=7"} ((fn[k](if (= k "sku-1"){:status 200 :body "stock=7"}{:status 404 :body "missing"})) "sku-1")))
