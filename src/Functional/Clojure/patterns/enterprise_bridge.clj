(defn enterprise-bridge-pattern [] (let [send #(str %1 ">" %2 ":" %3)] (and (= "kafka>ALERT:disk" (send "kafka" "ALERT" "disk"))(= "queue>REMINDER:backup" (send "queue" "REMINDER" "backup")))))
