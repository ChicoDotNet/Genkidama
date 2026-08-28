object MementoExample{fun run():Boolean{var state="draft";val snapshot=state;state="published";if(state!="published")return false;state=snapshot;return state=="draft"}}
