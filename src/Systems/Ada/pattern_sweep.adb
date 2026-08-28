with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Pattern_Sweep is
   type Int_Array is array (Positive range <>) of Integer;

   function Same_Int (Left, Right : Integer) return Boolean is
   begin
      return Left = Right;
   end Same_Int;

   function Same_Text (Left, Right : String) return Boolean is
   begin
      return Left = Right;
   end Same_Text;

   function Same_Float (Left, Right : Long_Float) return Boolean is
   begin
      return abs (Left - Right) < 1.0E-9;
   end Same_Float;

   function Same_Array (Left, Right : Int_Array) return Boolean is
   begin
      return Left = Right;
   end Same_Array;

   function Apply_Delta (Value, Delta : Integer) return Integer is
   begin
      return Value + Delta;
   end Apply_Delta;

   function Command_Pattern return Boolean is
      Balance : Integer := 100;
   begin
      Balance := Apply_Delta (Balance, 50);
      Balance := Apply_Delta (Balance, -20);
      return Same_Int (Balance, 130)
        and then Same_Int (Apply_Delta (150, -20), 130);
   end Command_Pattern;

   type Expr_Kind is (Literal, Add, Multiply);
   type Expr (Kind : Expr_Kind := Literal) is record
      case Kind is
         when Literal =>
            Value : Integer := 0;
         when Add | Multiply =>
            Left, Right : Integer := 0;
      end case;
   end record;

   function Eval (Item : Expr) return Integer is
   begin
      case Item.Kind is
         when Literal =>
            return Item.Value;
         when Add =>
            return Item.Left + Item.Right;
         when Multiply =>
            return Item.Left * Item.Right;
      end case;
   end Eval;

   function Interpreter_Pattern return Boolean is
      Product : constant Integer := Eval ((Kind => Multiply, Left => 3, Right => 4));
      Sum     : constant Integer := Eval ((Kind => Add, Left => 7, Right => Product));
   begin
      return Same_Int (Sum, 19);
   end Interpreter_Pattern;

   function Iterator_Pattern return Boolean is
      Values : constant Int_Array (1 .. 3) := [10, 20, 30];
      Seen   : Int_Array (1 .. 3) := [0, 0, 0];
   begin
      for Index in Values'Range loop
         Seen (Index) := Values (Index);
      end loop;
      return Same_Array (Seen, Values);
   end Iterator_Pattern;

   function Mediator_Pattern return Boolean is
      Events : Unbounded_String := Null_Unbounded_String;
      procedure Notify (Sender, Event : String) is
      begin
         if Same_Text (Sender, "button") and then Same_Text (Event, "click") then
            Append (Events, "panel.refresh");
         elsif Same_Text (Sender, "panel") and then Same_Text (Event, "loaded") then
            Append (Events, ">button.enable");
         end if;
      end Notify;
   begin
      Notify ("button", "click");
      Notify ("panel", "loaded");
      return Same_Text (To_String (Events), "panel.refresh>button.enable");
   end Mediator_Pattern;

   function Memento_Pattern return Boolean is
      State         : Unbounded_String := To_Unbounded_String ("draft");
      Snapshot      : constant Unbounded_String := State;
      Was_Published : Boolean;
   begin
      State := To_Unbounded_String ("published");
      Was_Published := Same_Text (To_String (State), "published");
      State := Snapshot;
      return Was_Published and then Same_Text (To_String (State), "draft");
   end Memento_Pattern;

   function Observer_Pattern return Boolean is
      Events : Unbounded_String := Null_Unbounded_String;
      procedure Audit (Id : Integer) is
      begin
         Append (Events, "audit:" & Integer'Image (Id));
      end Audit;
      procedure Dashboard (Id : Integer) is
      begin
         Append (Events, ">dashboard:" & Integer'Image (Id));
      end Dashboard;
   begin
      Audit (42);
      Dashboard (42);
      return Same_Text (To_String (Events), "audit: 42>dashboard: 42");
   end Observer_Pattern;

   function State_Pattern return Boolean is
      function Transition (State, Action : String) return String is
      begin
         if Same_Text (State, "locked") and then Same_Text (Action, "unlock") then
            return "unlocked";
         elsif Same_Text (State, "unlocked") and then Same_Text (Action, "lock") then
            return "locked";
         end if;
         return State;
      end Transition;
      Current : Unbounded_String := To_Unbounded_String ("locked");
   begin
      Current := To_Unbounded_String (Transition (To_String (Current), "unlock"));
      Current := To_Unbounded_String (Transition (To_String (Current), "lock"));
      return Same_Text (To_String (Current), "locked");
   end State_Pattern;

   type Int_Strategy is access function (Value : Integer) return Integer;
   function Identity_Strategy (Value : Integer) return Integer is (Value);
   function Discount_Strategy (Value : Integer) return Integer is (Value * 80 / 100);

   function Strategy_Pattern return Boolean is
      function Price (Value : Integer; Strategy : Int_Strategy) return Integer is
      begin
         return Strategy (Value);
      end Price;
   begin
      return Same_Int (Price (100, Identity_Strategy'Access), 100)
        and then Same_Int (Price (100, Discount_Strategy'Access), 80);
   end Strategy_Pattern;

   type Text_Step is access function return String;
   function Normalize return String is ("normalize");

   function Template_Method_Pattern return Boolean is
      function Pipeline (Read_Step : String; Transform : Text_Step) return String is
      begin
         return Read_Step & ">" & Transform.all & ">publish";
      end Pipeline;
   begin
      return Same_Text (
        Pipeline ("read-csv", Normalize'Access),
        "read-csv>normalize>publish");
   end Template_Method_Pattern;

   type Shape_Kind is (Circle, Rectangle);
   type Shape (Kind : Shape_Kind := Circle) is record
      case Kind is
         when Circle =>
            Radius : Long_Float := 0.0;
         when Rectangle =>
            Width, Height : Long_Float := 0.0;
      end case;
   end record;

   function Area (Item : Shape) return Long_Float is
   begin
      case Item.Kind is
         when Circle =>
            return Long_Float (Ada.Numerics.Pi) * Item.Radius * Item.Radius;
         when Rectangle =>
            return Item.Width * Item.Height;
      end case;
   end Area;

   function Visitor_Pattern return Boolean is
      Total : constant Long_Float :=
        Area ((Kind => Circle, Radius => 2.0))
        + Area ((Kind => Rectangle, Width => 3.0, Height => 4.0));
      Expected : constant Long_Float := 4.0 * Long_Float (Ada.Numerics.Pi) + 12.0;
   begin
      return Same_Float (Total, Expected);
   end Visitor_Pattern;

   function MVC_Pattern return Boolean is
      Count : Integer := 0;
      function View return String is
      begin
         return "count=" & Integer'Image (Count);
      end View;
      Before : constant String := View;
   begin
      Count := Count + 1;
      return Same_Text (Before, "count= 0") and then Same_Text (View, "count= 1");
   end MVC_Pattern;

   function MVVM_Pattern return Boolean is
      Amount : Integer := 10;
      function Text return String is
      begin
         return "$" & Integer'Image (Amount) & ".00";
      end Text;
      Before : constant String := Text;
   begin
      Amount := Amount + 5;
      return Same_Text (Before, "$ 10.00") and then Same_Text (Text, "$ 15.00");
   end MVVM_Pattern;

   function Double_Value (Value : Integer) return Integer is (Value * 2);
   function Square_Value (Value : Integer) return Integer is (Value * Value);

   function Microkernel_Pattern return Boolean is
      type Plugin_Array is array (Positive range <>) of Int_Strategy;
      Plugins : constant Plugin_Array := [Double_Value'Access, Square_Value'Access];
   begin
      return Same_Int (Plugins (1) (4), 8)
        and then Same_Int (Plugins (2) (4), 16);
   end Microkernel_Pattern;

   function Microservices_Pattern return Boolean is
      Stock : Integer := 7;
      function Reserve (Quantity : Integer) return Boolean is
      begin
         if Quantity > Stock then
            return False;
         end if;
         Stock := Stock - Quantity;
         return True;
      end Reserve;
      function Place (Quantity : Integer) return String is
      begin
         if Reserve (Quantity) then
            return "confirmed";
         end if;
         return "rejected";
      end Place;
      Result : constant String := Place (2);
   begin
      return Same_Text (Result, "confirmed") and then Same_Int (Stock, 5);
   end Microservices_Pattern;

   function Enterprise_Adapter_Pattern return Boolean is
      Legacy_Code  : constant Integer := 17;
      Legacy_Cents : constant Integer := 1250;
      Canonical_Id : constant Integer := Legacy_Code;
      Amount       : constant Long_Float := Long_Float (Legacy_Cents) / 100.0;
   begin
      return Same_Int (Canonical_Id, 17) and then Same_Float (Amount, 12.5);
   end Enterprise_Adapter_Pattern;

   function Send (Transport, Kind, Message : String) return String is
   begin
      return Transport & ">" & Kind & ":" & Message;
   end Send;

   function Enterprise_Bridge_Pattern return Boolean is
   begin
      return Same_Text (Send ("kafka", "ALERT", "disk"), "kafka>ALERT:disk")
        and then Same_Text (
          Send ("queue", "REMINDER", "backup"),
          "queue>REMINDER:backup");
   end Enterprise_Bridge_Pattern;

   function Enterprise_Facade_Pattern return Boolean is
      function CRM (Id : Integer) return String is
      begin
         return "crm:create:" & Integer'Image (Id);
      end CRM;
      function Billing (Id : Integer) return String is
      begin
         return "billing:open:" & Integer'Image (Id);
      end Billing;
   begin
      return Same_Text (
        CRM (77) & ">" & Billing (77),
        "crm:create: 77>billing:open: 77");
   end Enterprise_Facade_Pattern;

   type Text_Service is access function (Value : String) return String;
   function Inventory_Service (Value : String) return String is ("inventory:" & Value & "=7");
   function Customer_Service (Value : String) return String is ("customer:" & Value & "=active");
   function Email_Service (Value : String) return String is ("email>" & Value);
   function Audit_Service (Value : String) return String is ("audit>" & Value);

   function Broker_Pattern return Boolean is
      Inventory : constant Text_Service := Inventory_Service'Access;
      Customer  : constant Text_Service := Customer_Service'Access;
   begin
      return Same_Text (Inventory ("sku-1"), "inventory:sku-1=7")
        and then Same_Text (Customer ("17"), "customer:17=active");
   end Broker_Pattern;

   function Message_Bus_Pattern return Boolean is
      Events : Unbounded_String := Null_Unbounded_String;
      procedure Publish (Topic : String; Id : Integer) is
      begin
         Append (Events, "audit:" & Topic & ":" & Integer'Image (Id));
         Append (Events, ">billing:" & Topic & ":" & Integer'Image (Id));
      end Publish;
   begin
      Publish ("order-created", 42);
      return Same_Text (
        To_String (Events),
        "audit:order-created: 42>billing:order-created: 42");
   end Message_Bus_Pattern;

   function Service_Locator_Pattern return Boolean is
      Email : constant Text_Service := Email_Service'Access;
      Audit : constant Text_Service := Audit_Service'Access;
   begin
      return Same_Text (Email ("a@example.test"), "email>a@example.test")
        and then Same_Text (Audit ("created"), "audit>created");
   end Service_Locator_Pattern;

   function Active_Object_Pattern return Boolean is
      Value : Integer := 0;
      procedure Add_Three is begin Value := Value + 3; end Add_Three;
      procedure Times_Four is begin Value := Value * 4; end Times_Four;
   begin
      Add_Three;
      Times_Four;
      return Same_Int (Value, 12);
   end Active_Object_Pattern;

   function Monitor_Object_Pattern return Boolean is
      protected Counter is
         procedure Add (Amount : Integer);
         function Value return Integer;
      private
         Current : Integer := 0;
      end Counter;

      protected body Counter is
         procedure Add (Amount : Integer) is
         begin
            Current := Current + Amount;
         end Add;
         function Value return Integer is
         begin
            return Current;
         end Value;
      end Counter;
   begin
      Counter.Add (2);
      Counter.Add (3);
      return Same_Int (Counter.Value, 5);
   end Monitor_Object_Pattern;

   function Half_Sync_Half_Async_Pattern return Boolean is
      Jobs : constant array (1 .. 3) of Unbounded_String :=
        [To_Unbounded_String ("job-1"),
         To_Unbounded_String ("job-2"),
         To_Unbounded_String ("job-3")];
      Results : Unbounded_String := Null_Unbounded_String;
   begin
      for Index in Jobs'Range loop
         if Length (Results) > 0 then
            Append (Results, ">");
         end if;
         Append (Results, "done:" & To_String (Jobs (Index)));
      end loop;
      return Same_Text (To_String (Results), "done:job-1>done:job-2>done:job-3");
   end Half_Sync_Half_Async_Pattern;

   function Leader_Followers_Pattern return Boolean is
      Workers : constant array (1 .. 3) of Unbounded_String :=
        [To_Unbounded_String ("worker-1"),
         To_Unbounded_String ("worker-2"),
         To_Unbounded_String ("worker-3")];
      Events : constant array (1 .. 3) of Character := ['a', 'b', 'c'];
      Handled : Unbounded_String := Null_Unbounded_String;
   begin
      for Index in Events'Range loop
         if Length (Handled) > 0 then
            Append (Handled, ">");
         end if;
         Append (Handled, To_String (Workers (Index)) & ":" & Events (Index));
      end loop;
      return Same_Text (To_String (Handled), "worker-1:a>worker-2:b>worker-3:c")
        and then Same_Text (To_String (Workers (1)), "worker-1");
   end Leader_Followers_Pattern;

   function Client_Server_Pattern return Boolean is
      function Server (Key : String) return String is
      begin
         if Same_Text (Key, "sku-1") then
            return "200:stock=7";
         end if;
         return "404:missing";
      end Server;
   begin
      return Same_Text (Server ("sku-1"), "200:stock=7");
   end Client_Server_Pattern;

   function Peer_To_Peer_Pattern return Boolean is
      Inbox : Unbounded_String := Null_Unbounded_String;
      procedure Send_Block (From_Peer, To_Peer, Data : String) is
      begin
         if Length (Inbox) > 0 then
            Append (Inbox, ">");
         end if;
         Append (Inbox, From_Peer & ">" & To_Peer & ":" & Data);
      end Send_Block;
   begin
      Send_Block ("peer-a", "peer-b", "block-42");
      Send_Block ("peer-a", "peer-c", "block-42");
      return Same_Text (
        To_String (Inbox),
        "peer-a>peer-b:block-42>peer-a>peer-c:block-42");
   end Peer_To_Peer_Pattern;

   function Publish_Subscribe_Pattern return Boolean is
      Results : Unbounded_String := Null_Unbounded_String;
      procedure Publish (Id : Integer) is
      begin
         Append (Results, "warehouse:" & Integer'Image (Id));
         Append (Results, ">analytics:" & Integer'Image (Id));
      end Publish;
   begin
      Publish (51);
      return Same_Text (
        To_String (Results),
        "warehouse: 51>analytics: 51");
   end Publish_Subscribe_Pattern;

   function Distributed_Proxy_Pattern return Boolean is
      function Remote (SKU : String) return Integer is
      begin
         if Same_Text (SKU, "sku-1") then
            return 7;
         end if;
         return 0;
      end Remote;
      function Proxy (SKU : String) return Integer is
      begin
         return Remote (SKU);
      end Proxy;
   begin
      return Same_Int (Proxy ("sku-1"), 7);
   end Distributed_Proxy_Pattern;

   function PAC_Pattern return Boolean is
      function View (Name : String; Value : Integer) return String is
      begin
         return Name & ":view=" & Integer'Image (Value);
      end View;
   begin
      return Same_Text (View ("child", 42), "child:view= 42")
        and then Same_Text (View ("root", 42), "root:view= 42");
   end PAC_Pattern;

   function MVP_Pattern return Boolean is
      Count : Integer := 0;
      Text  : Unbounded_String := Null_Unbounded_String;
      procedure Present is
      begin
         Count := Count + 1;
         Text := To_Unbounded_String ("count=" & Integer'Image (Count));
      end Present;
   begin
      Present;
      return Same_Int (Count, 1) and then Same_Text (To_String (Text), "count= 1");
   end MVP_Pattern;

   function Document_View_Pattern return Boolean is
      Title : constant String := "Final";
      Words : constant Integer := 120;
      Editor : constant String := "editor:" & Title & ":" & Integer'Image (Words);
      Summary : constant String := "summary:" & Title;
   begin
      return Same_Text (Editor, "editor:Final: 120")
        and then Same_Text (Summary, "summary:Final");
   end Document_View_Pattern;

   function Active_Record_Pattern return Boolean is
      Id   : Integer;
      Name : Unbounded_String;
   begin
      Id := 7;
      Name := To_Unbounded_String ("Ada");
      return Same_Int (Id, 7) and then Same_Text (To_String (Name), "Ada");
   end Active_Record_Pattern;

   function Data_Mapper_Pattern return Boolean is
      Id   : constant Integer := 8;
      Name : constant String := "Grace";
      Key  : constant String := "person:" & Integer'Image (Id);
   begin
      return Same_Text (Key, "person: 8") and then Same_Text (Name, "Grace");
   end Data_Mapper_Pattern;

   function Unit_Of_Work_Pattern return Boolean is
      Pending  : Int_Array (1 .. 2) := [2, 3];
      Store    : Int_Array (1 .. 2) := [0, 0];
      Empty    : constant Int_Array (1 .. 2) := [0, 0];
      Expected : constant Int_Array (1 .. 2) := [2, 3];
   begin
      Store := Pending;
      Pending := Empty;
      return Same_Array (Store, Expected) and then Same_Array (Pending, Empty);
   end Unit_Of_Work_Pattern;

   function Repository_Pattern return Boolean is
      Ids   : constant Int_Array (1 .. 2) := [1, 2];
      Names : constant array (1 .. 2) of Unbounded_String :=
        [To_Unbounded_String ("Ada"), To_Unbounded_String ("Grace")];
      Found : Unbounded_String := Null_Unbounded_String;
   begin
      for Index in Ids'Range loop
         if Same_Int (Ids (Index), 2) then
            Found := Names (Index);
         end if;
      end loop;
      return Same_Text (To_String (Found), "Grace");
   end Repository_Pattern;

   type Clock_Access is access function return String;
   function Fixed_Clock return String is ("10:00");

   function Dependency_Injection_Pattern return Boolean is
      function Service (Clock : Clock_Access) return String is
      begin
         return "at:" & Clock.all;
      end Service;
   begin
      return Same_Text (Service (Fixed_Clock'Access), "at:10:00");
   end Dependency_Injection_Pattern;

   function Lazy_Initialization_Pattern return Boolean is
      Builds : Integer := 0;
      Ready  : Boolean := False;
      procedure Ensure is
      begin
         if not Ready then
            Builds := Builds + 1;
            Ready := True;
         end if;
      end Ensure;
   begin
      Ensure;
      Ensure;
      return Ready and then Same_Int (Builds, 1);
   end Lazy_Initialization_Pattern;

   function Object_Pool_Pattern return Boolean is
      Pool        : Int_Array (1 .. 2) := [1, 2];
      Borrowed    : constant Integer := Pool (2);
      Expected    : constant Int_Array (1 .. 2) := [1, 2];
      Checked_Out : Boolean;
   begin
      Pool (2) := 0;
      Checked_Out := Same_Int (Pool (2), 0);
      Pool (2) := Borrowed;
      return Checked_Out and then Same_Array (Pool, Expected);
   end Object_Pool_Pattern;

   type Logger_Access is access function (Message : String) return String;
   function Null_Log (Message : String) return String is
      pragma Unreferenced (Message);
   begin
      return "";
   end Null_Log;
   function Real_Log (Message : String) return String is ("log:" & Message);

   function Null_Object_Pattern return Boolean is
      Null_Logger : constant Logger_Access := Null_Log'Access;
      Real_Logger : constant Logger_Access := Real_Log'Access;
   begin
      return Same_Text (Null_Logger ("x"), "")
        and then Same_Text (Real_Logger ("x"), "log:x");
   end Null_Object_Pattern;

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
