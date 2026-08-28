object MementoExample { def run:Boolean={var s="draft";val snap=s;s="published";s=snap;s=="draft"} }
