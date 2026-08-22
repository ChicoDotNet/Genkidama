with Ada.Text_IO; use Ada.Text_IO;

procedure Decorator is
   type Render_Access is access function return String;

   function Plain return String is ("alert");

   function Audit (Inner : Render_Access) return String is
      ("audit(" & Inner.all & ")");

   function Encrypt (Inner : Render_Access) return String is
      ("enc(" & Inner.all & ")");

   function Audited return String is (Audit (Plain'Access));
   function Encrypted return String is (Encrypt (Plain'Access));
   function Encrypted_Inner return String is (Encrypt (Plain'Access));
   function Stacked return String is (Audit (Encrypted_Inner'Access));
begin
   Put_Line ("base=" & Plain);
   Put_Line ("audit=" & Audited);
   Put_Line ("encrypted=" & Encrypted);
   Put_Line ("stacked=" & Stacked);
end Decorator;
