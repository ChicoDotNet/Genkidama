with Ada.Text_IO; use Ada.Text_IO;

procedure Singleton is
   package Registry is
      type State is record
         Count : Natural := 0;
      end record;
      type State_Access is access all State;
      function Instance return State_Access;
   end Registry;

   package body Registry is
      Shared : aliased State;
      function Instance return State_Access is
      begin
         return Shared'Access;
      end Instance;
   end Registry;

   First  : constant Registry.State_Access := Registry.Instance;
   Second : constant Registry.State_Access := Registry.Instance;
begin
   First.Count := First.Count + 1;
   Put_Line ("same=" & Boolean'Image (First = Second));
   Put_Line ("count=" & Natural'Image (Second.Count));
end Singleton;
