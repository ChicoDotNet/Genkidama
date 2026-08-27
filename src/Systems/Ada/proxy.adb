with Ada.Text_IO; use Ada.Text_IO;

procedure Proxy is
   type Remote_Document_Store is record
      Fetches : Natural := 0;
   end record;

   function Get_Document
     (Store : in out Remote_Document_Store; Id : Positive) return String is
   begin
      Store.Fetches := Store.Fetches + 1;
      return "doc(" & Positive'Image (Id) (2 .. Positive'Image (Id)'Last) & ")";
   end Get_Document;

   type Document_Store_Proxy is record
      Backend_Created : Boolean := False;
      Backend         : Remote_Document_Store;
      Has_Cache       : Boolean := False;
      Cached_Id       : Positive := 1;
   end record;

   function Get_Document
     (Store : in out Document_Store_Proxy; Id : Positive) return String is
   begin
      if Store.Has_Cache and then Store.Cached_Id = Id then
         return "doc(" & Positive'Image (Id) (2 .. Positive'Image (Id)'Last) & ")";
      end if;

      Store.Backend_Created := True;
      Store.Cached_Id := Id;
      Store.Has_Cache := True;
      return Get_Document (Store.Backend, Id);
   end Get_Document;

   Store        : Document_Store_Proxy;
   First_Value  : constant String := Get_Document (Store, 42);
   Second_Value : constant String := Get_Document (Store, 42);
begin
   Put_Line
     ("backend=" & (if Store.Backend_Created then "1" else "0") &
      ";fetches=" & Natural'Image (Store.Backend.Fetches) (2 .. Natural'Image (Store.Backend.Fetches)'Last) &
      ";first=" & First_Value & ";second=" & Second_Value);
end Proxy;
