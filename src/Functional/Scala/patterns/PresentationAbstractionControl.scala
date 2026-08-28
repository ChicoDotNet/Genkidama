object PresentationAbstractionControlExample { def run:Boolean={def view(n:String,v:Int)=s"$n:view=$v";view("child",42)=="child:view=42"&&view("root",42)=="root:view=42"} }
