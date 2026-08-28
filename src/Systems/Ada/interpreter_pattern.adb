function Interpreter_Pattern return Boolean is
   type Expr_Kind is (Literal, Add, Multiply);
   type Expr (Kind : Expr_Kind := Literal) is record
      case Kind is
         when Literal =>
            Value : Integer := 0;
         when Add | Multiply =>
            Left, Right : Integer := 0;
      end case;
   end record;

   function Eval (Item : Expr) return Integer is
   begin
      case Item.Kind is
         when Literal  => return Item.Value;
         when Add      => return Item.Left + Item.Right;
         when Multiply => return Item.Left * Item.Right;
      end case;
   end Eval;

   Product : constant Integer := Eval ((Kind => Multiply, Left => 3, Right => 4));
   Sum     : constant Integer := Eval ((Kind => Add, Left => 7, Right => Product));
begin
   return Sum = 19;
end Interpreter_Pattern;
