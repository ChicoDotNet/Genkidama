module StateExample
let run ()=let transition state action=if state="locked"&&action="unlock" then "unlocked" elif state="unlocked"&&action="lock" then "locked" else state in transition(transition "locked" "unlock")"lock"="locked"
