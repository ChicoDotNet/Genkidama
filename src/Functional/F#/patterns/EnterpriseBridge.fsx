module EnterpriseBridgeExample
let run ()=let send transport kind message=$"{transport}>{kind}:{message}" in send "kafka" "ALERT" "disk"="kafka>ALERT:disk"&&send "queue" "REMINDER" "backup"="queue>REMINDER:backup"
