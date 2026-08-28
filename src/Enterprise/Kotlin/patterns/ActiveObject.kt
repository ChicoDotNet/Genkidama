object ActiveObjectExample{fun run():Boolean{var v=0;val q=listOf<()->Unit>({v+=3},{v*=4});val before=v;q.forEach{it()};return before==0&&v==12}}
