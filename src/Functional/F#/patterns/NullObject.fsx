module NullObjectExample
let run ()=let nullLog _="" in let realLog message=$"log:{message}" in nullLog "x"=""&&realLog "x"="log:x"
