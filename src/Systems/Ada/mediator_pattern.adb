with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Mediator_Pattern return Boolean is
   type Receiver_Access is access procedure (Sender, Message : String);

   type Colleague is record
      Name    : Unbounded_String;
      Receive : Receiver_Access;
   end record;

   type Colleague_Array is array (Positive range <>) of Colleague;

   Events : Unbounded_String := Null_Unbounded_String;

   procedure Inventory_Receive (Sender, Message : String) is
   begin
      if Sender /= "payment" or else Message /= "paid" then
         raise Program_Error with "unexpected inventory message";
      end if;
      Append (Events, "inventory<-payment:paid");
   end Inventory_Receive;

   procedure Payment_Receive (Sender, Message : String) is
   begin
      if Sender /= "inventory" or else Message /= "reserved" then
         raise Program_Error with "unexpected payment message";
      end if;
      Append (Events, ">payment<-inventory:reserved");
   end Payment_Receive;

   Colleagues : constant Colleague_Array :=
     [1 => (To_Unbounded_String ("inventory"), Inventory_Receive'Access),
      2 => (To_Unbounded_String ("payment"), Payment_Receive'Access)];

   procedure Send (Sender, Recipient, Message : String) is
   begin
      for Item of Colleagues loop
         if To_String (Item.Name) = Recipient then
            Item.Receive.all (Sender, Message);
            return;
         end if;
      end loop;
      raise Constraint_Error with "unknown colleague: " & Recipient;
   end Send;

   Unknown_Rejected : Boolean := False;
begin
   Send ("payment", "inventory", "paid");
   Send ("inventory", "payment", "reserved");

   begin
      Send ("payment", "unknown", "ignored");
   exception
      when Constraint_Error =>
         Unknown_Rejected := True;
   end;

   return Unknown_Rejected
     and then To_String (Events) = "inventory<-payment:paid>payment<-inventory:reserved";
end Mediator_Pattern;
