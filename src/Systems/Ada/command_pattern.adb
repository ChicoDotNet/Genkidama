function Command_Pattern return Boolean is
   type Command_Kind is (Deposit, Withdraw);
   type Command is record
      Kind   : Command_Kind;
      Amount : Integer;
   end record;
   type Command_Array is array (Positive range <>) of Command;

   function Execute (Balance : Integer; Item : Command) return Integer is
   begin
      case Item.Kind is
         when Deposit  => return Balance + Item.Amount;
         when Withdraw => return Balance - Item.Amount;
      end case;
   end Execute;

   Queue   : constant Command_Array :=
     [(Kind => Deposit, Amount => 50), (Kind => Withdraw, Amount => 20)];
   Balance : Integer := 100;
begin
   for Item of Queue loop
      Balance := Execute (Balance, Item);
   end loop;
   return Balance = 130 and then Execute (150, Queue (2)) = 130;
end Command_Pattern;
