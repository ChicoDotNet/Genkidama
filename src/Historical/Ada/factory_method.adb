with Ada.Text_IO; use Ada.Text_IO;

procedure Factory_Method is
   type Database_Kind is (Postgres, MySQL);
   type Factory_Method_Access is access function return Database_Kind;

   function Create_Postgres return Database_Kind is (Postgres);
   function Create_MySQL return Database_Kind is (MySQL);

   procedure Connect (Database : Database_Kind) is
   begin
      case Database is
         when Postgres => Put_Line ("PostgreSQL connect");
         when MySQL    => Put_Line ("MySQL connect");
      end case;
   end Connect;

   procedure Query (Database : Database_Kind) is
   begin
      case Database is
         when Postgres => Put_Line ("PostgreSQL query");
         when MySQL    => Put_Line ("MySQL query");
      end case;
   end Query;

   procedure Use_Database (Create_Database : Factory_Method_Access) is
      Database : constant Database_Kind := Create_Database.all;
   begin
      Connect (Database);
      Query (Database);
   end Use_Database;

begin
   Use_Database (Create_Postgres'Access);
   Use_Database (Create_MySQL'Access);
end Factory_Method;
