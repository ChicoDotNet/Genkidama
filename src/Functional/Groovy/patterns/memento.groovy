def state=[text:'draft'];def snap=state.clone();state.text='edited';state=snap;assert state.text=='draft'
