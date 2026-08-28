let ()=let state=ref"draft" in let snapshot=!state in state:="edited";state:=snapshot;assert(!state="draft")
