object DataMapperExample { def run:Boolean={val p=(8,"Grace");val row=(s"person:${p._1}",p._2);row._1=="person:8"&&row._2=="Grace"} }
