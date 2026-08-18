with Ada.Text_IO; use Ada.Text_IO;

procedure Factory_Method is
   type Database_Action is access procedure;

   type Database_Product is record
      Connect : Database_Action;
      Query   : Database_Action;
   end record;

   type Factory_Method_Access is access function return Database_Product;

   procedure Postgres_Connect is
   begin
      Put_Line ("PostgreSQL connect");
   end Postgres_Connect;

   procedure Postgres_Query is
   begin
      Put_Line ("PostgreSQL query");
   end Postgres_Query;

   procedure MySQL_Connect is
   begin
      Put_Line ("MySQL connect");
   end MySQL_Connect;

   procedure MySQL_Query is
   begin
      Put_Line ("MySQL query");
   end MySQL_Query;

   function Create_Postgres return Database_Product is
     (Connect => Postgres_Connect'Access,
      Query   => Postgres_Query'Access);

   function Create_MySQL return Database_Product is
     (Connect => MySQL_Connect'Access,
      Query   => MySQL_Query'Access);

   procedure Use_Database (Create_Database : Factory_Method_Access) is
      Database : constant Database_Product := Create_Database.all;
   begin
      Database.Connect.all;
      Database.Query.all;
   end Use_Database;

begin
   Use_Database (Create_Postgres'Access);
   Use_Database (Create_MySQL'Access);
end Factory_Method;
