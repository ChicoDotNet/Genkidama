object TemplateMethodExample{fun run():Boolean{val pipeline={read:String,transform:()->String->"$read>${transform()}>publish"};return pipeline("read-csv"){"normalize"}=="read-csv>normalize>publish"}}
