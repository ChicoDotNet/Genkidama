with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Composite is
   type Node_Kind is (File_Node, Folder_Node);
   type Node;
   type Node_Access is access all Node;
   type Node_Array is array (Positive range <>) of Node_Access;
   type Node_Array_Access is access Node_Array;

   type Node (Kind : Node_Kind := File_Node) is record
      case Kind is
         when File_Node =>
            Bytes : Integer := 0;
         when Folder_Node =>
            Children : Node_Array_Access;
      end case;
   end record;

   function Size (Item : Node) return Integer is
      Total : Integer := 0;
   begin
      case Item.Kind is
         when File_Node =>
            return Item.Bytes;
         when Folder_Node =>
            for Child of Item.Children.all loop
               Total := Total + Size (Child.all);
            end loop;
            return Total;
      end case;
   end Size;

   function Image (Value : Integer) return String is
     (Ada.Strings.Fixed.Trim (Integer'Image (Value), Ada.Strings.Both));

   Readme : constant Node_Access := new Node'(Kind => File_Node, Bytes => 2);
   Api    : constant Node_Access := new Node'(Kind => File_Node, Bytes => 3);
   Guide  : constant Node_Access := new Node'(Kind => File_Node, Bytes => 5);
   Docs   : constant Node_Access := new Node'
     (Kind => Folder_Node,
      Children => new Node_Array'(1 => Api, 2 => Guide));
   Root   : constant Node_Access := new Node'
     (Kind => Folder_Node,
      Children => new Node_Array'(1 => Readme, 2 => Docs));
begin
   Put_Line ("leaf=" & Image (Size (Readme.all)));
   Put_Line ("docs=" & Image (Size (Docs.all)));
   Put_Line ("root=" & Image (Size (Root.all)));
end Composite;
