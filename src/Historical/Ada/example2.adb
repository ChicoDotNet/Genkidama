with Ada.Text_IO; use Ada.Text_IO;

procedure Example2 is
   type Database is abstract tagged null record;
   procedure Connect (DB : in out Database) is abstract;
   procedure Query (DB : in out Database) is abstract;

   type Postgres is new Database with null record;
   procedure Connect (DB : in out Postgres);
   procedure Query (DB : in out Postgres);

   type MySQL is new Database with null record;
   procedure Connect (DB : in out MySQL);
   procedure Query (DB : in out MySQL);

   type Database_Factory is abstract tagged null record;
   function Create_Database (Factory : in Database_Factory) return Database'Class is abstract;

   type Postgres_Factory is new Database_Factory with null record;
   function Create_Database (Factory : in Postgres_Factory) return Database'Class;

   type MySQL_Factory is new Database_Factory with null record;
   function Create_Database (Factory : in MySQL_Factory) return Database'Class;

   procedure Connect (DB : in out Postgres) is
   begin
      Put_Line ("Connecting to PostgreSQL");
   end Connect;

   procedure Query (DB : in out Postgres) is
   begin
      Put_Line ("Querying PostgreSQL");
   end Query;

   procedure Connect (DB : in out MySQL) is
   begin
      Put_Line ("Connecting to MySQL");
   end Connect;

   procedure Query (DB : in out MySQL) is
   begin
      Put_Line ("Querying MySQL");
   end Query;

   function Create_Database (Factory : in Postgres_Factory) return Database'Class is
   begin
      return new Postgres;
   end Create_Database;

   function Create_Database (Factory : in MySQL_Factory) return Database'Class is
   begin
      return new MySQL;
   end Create_Database;

   procedure Use_Database (Factory : in Database_Factory) is
      DB : Database'Class := Factory.Create_Database;
   begin
      DB.Connect;
      DB.Query;
   end Use_Database;

begin
   declare
      Postgres_Fact : Postgres_Factory;
      MySQL_Fact : MySQL_Factory;
   begin
      Use_Database (Postgres_Fact);
      Use_Database (MySQL_Fact);
   end;
end Example2;
