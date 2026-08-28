object LazyInitializationExample{fun run():Boolean{var builds=0;val value by lazy{builds++;"ready"};val a=value;val b=value;return a=="ready"&&b=="ready"&&builds==1}}
