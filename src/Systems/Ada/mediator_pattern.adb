with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Mediator_Pattern return Boolean is
   Events : Unbounded_String := Null_Unbounded_String;

   procedure Notify (Sender, Event_Name : String) is
   begin
      if Sender = "button" and then Event_Name = "click" then
         Append (Events, "panel.refresh");
      elsif Sender = "panel" and then Event_Name = "loaded" then
         Append (Events, ">button.enable");
      end if;
   end Notify;
begin
   Notify ("button", "click");
   Notify ("panel", "loaded");
   return To_String (Events) = "panel.refresh>button.enable";
end Mediator_Pattern;
