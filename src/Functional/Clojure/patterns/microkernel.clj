(defn microkernel-pattern [] (let [p {:double #(* 2 %) :square #(* % %)}] (and (= 8 ((:double p) 4))(= 16 ((:square p) 4)))))
