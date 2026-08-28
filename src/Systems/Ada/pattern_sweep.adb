with Ada.Text_IO; use Ada.Text_IO;
with Command_Pattern;
with Interpreter_Pattern;
with Iterator_Pattern;
with Mediator_Pattern;
with Memento_Pattern;
with Observer_Pattern;
with State_Pattern;
with Strategy_Pattern;
with Template_Method_Pattern;
with Visitor_Pattern;
with MVC_Pattern;
with MVVM_Pattern;
with Microkernel_Pattern;
with Microservices_Pattern;
with Enterprise_Adapter_Pattern;
with Enterprise_Bridge_Pattern;
with Enterprise_Facade_Pattern;
with Broker_Pattern;
with Message_Bus_Pattern;
with Service_Locator_Pattern;
with Active_Object_Pattern;
with Monitor_Object_Pattern;
with Half_Sync_Half_Async_Pattern;
with Leader_Followers_Pattern;
with Client_Server_Pattern;
with Peer_To_Peer_Pattern;
with Publish_Subscribe_Pattern;
with Distributed_Proxy_Pattern;
with PAC_Pattern;
with MVP_Pattern;
with Document_View_Pattern;
with Active_Record_Pattern;
with Data_Mapper_Pattern;
with Unit_Of_Work_Pattern;
with Repository_Pattern;
with Dependency_Injection_Pattern;
with Lazy_Initialization_Pattern;
with Object_Pool_Pattern;
with Null_Object_Pattern;

procedure Pattern_Sweep is
   Count : Natural := 0;

   procedure Require (Ok : Boolean; Name : String) is
   begin
      if not Ok then
         raise Program_Error with "pattern failed: " & Name;
      end if;
      Count := Count + 1;
   end Require;
begin
   Require (Command_Pattern, "Command");
   Require (Interpreter_Pattern, "Interpreter");
   Require (Iterator_Pattern, "Iterator");
   Require (Mediator_Pattern, "Mediator");
   Require (Memento_Pattern, "Memento");
   Require (Observer_Pattern, "Observer");
   Require (State_Pattern, "State");
   Require (Strategy_Pattern, "Strategy");
   Require (Template_Method_Pattern, "Template Method");
   Require (Visitor_Pattern, "Visitor");
   Require (MVC_Pattern, "MVC");
   Require (MVVM_Pattern, "MVVM");
   Require (Microkernel_Pattern, "Microkernel");
   Require (Microservices_Pattern, "Microservices");
   Require (Enterprise_Adapter_Pattern, "Enterprise Adapter");
   Require (Enterprise_Bridge_Pattern, "Enterprise Bridge");
   Require (Enterprise_Facade_Pattern, "Enterprise Facade");
   Require (Broker_Pattern, "Broker");
   Require (Message_Bus_Pattern, "Message Bus");
   Require (Service_Locator_Pattern, "Service Locator");
   Require (Active_Object_Pattern, "Active Object");
   Require (Monitor_Object_Pattern, "Monitor Object");
   Require (Half_Sync_Half_Async_Pattern, "Half-Sync / Half-Async");
   Require (Leader_Followers_Pattern, "Leader / Followers");
   Require (Client_Server_Pattern, "Client-Server");
   Require (Peer_To_Peer_Pattern, "Peer-to-Peer");
   Require (Publish_Subscribe_Pattern, "Publish-Subscribe");
   Require (Distributed_Proxy_Pattern, "Distributed Proxy");
   Require (PAC_Pattern, "Presentation-Abstraction-Control");
   Require (MVP_Pattern, "Model-View-Presenter");
   Require (Document_View_Pattern, "Document-View");
   Require (Active_Record_Pattern, "Active Record");
   Require (Data_Mapper_Pattern, "Data Mapper");
   Require (Unit_Of_Work_Pattern, "Unit of Work");
   Require (Repository_Pattern, "Repository");
   Require (Dependency_Injection_Pattern, "Dependency Injection");
   Require (Lazy_Initialization_Pattern, "Lazy Initialization");
   Require (Object_Pool_Pattern, "Object Pool");
   Require (Null_Object_Pattern, "Null Object");

   if Count /= 39 then
      raise Program_Error with "expected 39 cases";
   end if;

   Put_Line ("Ada pattern sweep: 39/39 examples passed");
end Pattern_Sweep;
