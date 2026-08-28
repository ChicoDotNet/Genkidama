with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

function Leader_Followers_Pattern return Boolean is
   type Worker_Array is array (Positive range <>) of Unbounded_String;
   type Event_Array is array (Positive range <>) of Character;
   Workers : constant Worker_Array :=
     [To_Unbounded_String ("worker-1"),
      To_Unbounded_String ("worker-2"),
      To_Unbounded_String ("worker-3")];
   Events  : constant Event_Array := ['a', 'b', 'c'];
   Handled : Unbounded_String := Null_Unbounded_String;
begin
   for Index in Events'Range loop
      if Length (Handled) > 0 then
         Append (Handled, ">");
      end if;
      Append (Handled, To_String (Workers (Index)) & ":" & Events (Index));
   end loop;
   return To_String (Handled) = "worker-1:a>worker-2:b>worker-3:c"
     and then To_String (Workers (1)) = "worker-1";
end Leader_Followers_Pattern;
