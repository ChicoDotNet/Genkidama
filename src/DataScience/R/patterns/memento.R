# Memento: capture and restore state.
state<-list(text='draft'); snapshot<-state; state$text<-'edited'; state<-snapshot; stopifnot(state$text=='draft')
