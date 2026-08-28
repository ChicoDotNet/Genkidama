(defn object-pool-pattern [] (let [pool(atom[1 2]) x(last @pool)] (swap! pool pop)(swap! pool conj x)(and (= 2(count @pool))(some #{x}@pool))))
