(require '[clojure.string :as str])

(def handlers
  [{:name "faq" :accepts #(<= % 50)}
   {:name "billing" :accepts #(<= % 500)}
   {:name "escalation" :accepts (constantly true)}])

(defn route-request [amount chain]
  (loop [remaining chain
         visited []]
    (if-let [handler (first remaining)]
      (let [visited-now (conj visited (:name handler))]
        (if ((:accepts handler) amount)
          [visited-now (:name handler)]
          (recur (rest remaining) visited-now)))
      (throw (ex-info "No handler accepted the request" {:amount amount})))))

(let [amount 250
      [visited handled] (route-request amount handlers)]
  (println
    (format "visited=%s;handled=%s;result=refund(%d)"
            (str/join ">" visited)
            handled
            amount)))
