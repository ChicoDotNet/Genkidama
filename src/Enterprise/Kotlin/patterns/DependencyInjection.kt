object DependencyInjectionExample{fun run():Boolean{val service={clock:()->String->"at:${clock()}"};return service{"10:00"}=="at:10:00"}}
