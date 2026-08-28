object LazyInitializationExample { def run:Boolean={var builds=0;lazy val value={builds+=1;"ready"};val a=value;val b=value;a=="ready"&&b=="ready"&&builds==1} }
