function memento(); state.text='draft'; snap=state; state.text='edited'; state=snap; assert(strcmp(state.text,'draft')); end
