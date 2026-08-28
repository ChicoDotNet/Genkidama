program PatternSweep;
{$mode objfpc}{$H+}
uses
  Command_Pattern, Interpreter_Pattern, Iterator_Pattern, Mediator_Pattern,
  Memento_Pattern, Observer_Pattern, State_Pattern, Strategy_Pattern,
  Template_Method_Pattern, Visitor_Pattern, MVC_Pattern, MVVM_Pattern,
  Microkernel_Pattern, Microservices_Pattern, Enterprise_Adapter_Pattern,
  Enterprise_Bridge_Pattern, Enterprise_Facade_Pattern, Broker_Pattern,
  Message_Bus_Pattern, Service_Locator_Pattern, Active_Object_Pattern,
  Monitor_Object_Pattern, Half_Sync_Half_Async_Pattern, Leader_Followers_Pattern,
  Client_Server_Pattern, Peer_To_Peer_Pattern, Publish_Subscribe_Pattern,
  Distributed_Proxy_Pattern, PAC_Pattern, MVP_Pattern, Document_View_Pattern,
  Active_Record_Pattern, Data_Mapper_Pattern, Unit_Of_Work_Pattern,
  Repository_Pattern, Dependency_Injection_Pattern, Lazy_Initialization_Pattern,
  Object_Pool_Pattern, Null_Object_Pattern;

procedure Require(Ok:Boolean;const Name:String;var Count:Integer);
begin if not Ok then begin WriteLn(StdErr,'pattern failed: ',Name);Halt(1);end;Inc(Count);end;
var Count:Integer=0;
begin
  Require(Command_Pattern.Run,'Command',Count);
  Require(Interpreter_Pattern.Run,'Interpreter',Count);
  Require(Iterator_Pattern.Run,'Iterator',Count);
  Require(Mediator_Pattern.Run,'Mediator',Count);
  Require(Memento_Pattern.Run,'Memento',Count);
  Require(Observer_Pattern.Run,'Observer',Count);
  Require(State_Pattern.Run,'State',Count);
  Require(Strategy_Pattern.Run,'Strategy',Count);
  Require(Template_Method_Pattern.Run,'Template Method',Count);
  Require(Visitor_Pattern.Run,'Visitor',Count);
  Require(MVC_Pattern.Run,'MVC',Count);
  Require(MVVM_Pattern.Run,'MVVM',Count);
  Require(Microkernel_Pattern.Run,'Microkernel',Count);
  Require(Microservices_Pattern.Run,'Microservices',Count);
  Require(Enterprise_Adapter_Pattern.Run,'Enterprise Adapter',Count);
  Require(Enterprise_Bridge_Pattern.Run,'Enterprise Bridge',Count);
  Require(Enterprise_Facade_Pattern.Run,'Enterprise Facade',Count);
  Require(Broker_Pattern.Run,'Broker',Count);
  Require(Message_Bus_Pattern.Run,'Message Bus',Count);
  Require(Service_Locator_Pattern.Run,'Service Locator',Count);
  Require(Active_Object_Pattern.Run,'Active Object',Count);
  Require(Monitor_Object_Pattern.Run,'Monitor Object',Count);
  Require(Half_Sync_Half_Async_Pattern.Run,'Half-Sync / Half-Async',Count);
  Require(Leader_Followers_Pattern.Run,'Leader / Followers',Count);
  Require(Client_Server_Pattern.Run,'Client-Server',Count);
  Require(Peer_To_Peer_Pattern.Run,'Peer-to-Peer',Count);
  Require(Publish_Subscribe_Pattern.Run,'Publish-Subscribe',Count);
  Require(Distributed_Proxy_Pattern.Run,'Distributed Proxy',Count);
  Require(PAC_Pattern.Run,'Presentation-Abstraction-Control',Count);
  Require(MVP_Pattern.Run,'Model-View-Presenter',Count);
  Require(Document_View_Pattern.Run,'Document-View',Count);
  Require(Active_Record_Pattern.Run,'Active Record',Count);
  Require(Data_Mapper_Pattern.Run,'Data Mapper',Count);
  Require(Unit_Of_Work_Pattern.Run,'Unit of Work',Count);
  Require(Repository_Pattern.Run,'Repository',Count);
  Require(Dependency_Injection_Pattern.Run,'Dependency Injection',Count);
  Require(Lazy_Initialization_Pattern.Run,'Lazy Initialization',Count);
  Require(Object_Pool_Pattern.Run,'Object Pool',Count);
  Require(Null_Object_Pattern.Run,'Null Object',Count);
  if Count<>39 then Halt(1);
  WriteLn('Pascal pattern sweep: 39/39 examples passed');
end.
