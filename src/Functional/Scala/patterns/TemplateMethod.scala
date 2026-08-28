object TemplateMethodExample { def run:Boolean={def pipe(r:String,t:()=>String)=s"$r>${t()}>publish";pipe("read-csv",()=>"normalize")=="read-csv>normalize>publish"} }
