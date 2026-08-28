(defn mvvm-pattern [] (let [v (atom 10) text #(str "$" @v ".00") before (text)] (swap! v + 5)(and (= before "$10.00")(= (text) "$15.00"))))
