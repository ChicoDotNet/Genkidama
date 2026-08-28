object PresentationAbstractionControlExample{fun run():Boolean{val view={n:String,v:Int->"$n:view=$v"};return view("child",42)=="child:view=42"&&view("root",42)=="root:view=42"}}
