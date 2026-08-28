module EnterpriseBridgeExample
let run () =
    let send transport kind message =
        $"{transport}>{kind}:{message}"
    send "kafka" "ALERT" "disk" = "kafka>ALERT:disk"
    && send "queue" "REMINDER" "backup" = "queue>REMINDER:backup"
