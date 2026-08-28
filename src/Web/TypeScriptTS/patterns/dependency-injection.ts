function dependencyInjectionPattern(){const service=(clock:()=>string)=>`at:${clock()}`;return service(()=> '10:00')==='at:10:00'}
