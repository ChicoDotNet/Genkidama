(defn template-method-pattern [] (= "read-csv>normalize>publish" ((fn [r t](str r ">" (t) ">publish")) "read-csv" (constantly "normalize"))))
