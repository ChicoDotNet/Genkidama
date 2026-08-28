function Template_Method_Pattern return Boolean is
   type Text_Step is access function return String;
   function Normalize return String is ("normalize");

   function Pipeline (Read_Step : String; Transform : Text_Step) return String is
   begin
      return Read_Step & ">" & Transform.all & ">publish";
   end Pipeline;
begin
   return Pipeline ("read-csv", Normalize'Access) = "read-csv>normalize>publish";
end Template_Method_Pattern;
