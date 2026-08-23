(defn base-component [] "alert")
(defn audit-decorator [component] (fn [] (str "audit(" (component) ")")))
(defn encrypt-decorator [component] (fn [] (str "enc(" (component) ")")))

(let [base base-component
      audited (audit-decorator base)
      encrypted (encrypt-decorator base)
      stacked (audit-decorator (encrypt-decorator base))]
  (println (str "base=" (base)))
  (println (str "audit=" (audited)))
  (println (str "encrypted=" (encrypted)))
  (println (str "stacked=" (stacked))))
