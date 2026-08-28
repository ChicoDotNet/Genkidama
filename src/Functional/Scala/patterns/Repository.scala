object RepositoryExample { def run:Boolean=List(1->"Ada",2->"Grace").find(_._1==2).exists(_._2=="Grace") }
