object StateExample { def run:Boolean={def t(s:String,a:String)=if(s=="locked"&&a=="unlock")"unlocked"else if(s=="unlocked"&&a=="lock")"locked"else s;t(t("locked","unlock"),"lock")=="locked"} }
