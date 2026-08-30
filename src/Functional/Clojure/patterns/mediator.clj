(defn checkout-mediator [routes]
  (fn [sender recipient message]
    (if-let [receive (get routes recipient)]
      (receive sender message)
      (throw (ex-info "unknown colleague" {:recipient recipient})))))

(defn mediator-pattern []
  (let [events (atom [])
        mediator (checkout-mediator
                  {:inventory #(swap! events conj (str "inventory<-" (name %1) ":" (name %2)))
                   :payment #(swap! events conj (str "payment<-" (name %1) ":" (name %2)))})]
    (mediator :payment :inventory :paid)
    (mediator :inventory :payment :reserved)
    (= @events ["inventory<-payment:paid"
                "payment<-inventory:reserved"])))

(assert (mediator-pattern))
