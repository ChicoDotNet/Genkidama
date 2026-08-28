module TemplateMethodExample
let run ()=let pipeline read transform=$"{read}>{transform()}>publish" in pipeline "read-csv"(fun()->"normalize")="read-csv>normalize>publish"
