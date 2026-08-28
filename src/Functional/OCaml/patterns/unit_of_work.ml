let ()=let pending=ref[1]and database=ref[]in database:=!pending;pending:=[];assert(!database=[1]&&!pending=[])
