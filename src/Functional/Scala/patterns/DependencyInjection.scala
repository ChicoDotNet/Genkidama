object DependencyInjectionExample { def run:Boolean={def service(clock:()=>String)=s"at:${clock()}";service(()=>"10:00")=="at:10:00"} }
