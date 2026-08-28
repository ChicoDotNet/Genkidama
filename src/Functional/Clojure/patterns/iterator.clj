(defn iterator-pattern [] (= [10 20 30] (vec (iterator-seq (.iterator [10 20 30])))))
