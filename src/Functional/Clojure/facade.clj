(defn authenticate [user] (str "auth(" user ")"))
(defn reserve [sku] (str "reserve(" sku ")"))
(defn charge [cents] (str "charge(" cents ")"))

(defn checkout-facade [user sku cents]
  (str (authenticate user) ">" (reserve sku) ">" (charge cents)))

(println (str "checkout=" (checkout-facade "alice" "SKU-42" 499)))
