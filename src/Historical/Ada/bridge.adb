with Ada.Text_IO; use Ada.Text_IO;

procedure Bridge is
   type Action is access function return String;

   type Device is record
      Power_On : Action;
      Mute     : Action;
   end record;

   function TV_On return String is ("TV:on");
   function TV_Mute return String is ("TV:muted");
   function Radio_On return String is ("Radio:on");
   function Radio_Mute return String is ("Radio:muted");

   function Activate_Basic (Target : Device) return String is
     (Target.Power_On.all);

   function Activate_Mute (Target : Device) return String is
     (Target.Mute.all);

   TV : constant Device := (Power_On => TV_On'Access, Mute => TV_Mute'Access);
   Radio : constant Device :=
     (Power_On => Radio_On'Access, Mute => Radio_Mute'Access);
begin
   Put_Line ("basic-tv=" & Activate_Basic (TV));
   Put_Line ("basic-radio=" & Activate_Basic (Radio));
   Put_Line ("mute-tv=" & Activate_Mute (TV));
   Put_Line ("mute-radio=" & Activate_Mute (Radio));
end Bridge;
