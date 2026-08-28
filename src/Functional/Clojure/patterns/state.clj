(defn state-pattern [] (let [t #(cond (and (= %1 :locked)(= %2 :unlock)) :unlocked (and (= %1 :unlocked)(= %2 :lock)) :locked :else %1)] (= :locked (t (t :locked :unlock) :lock))))
