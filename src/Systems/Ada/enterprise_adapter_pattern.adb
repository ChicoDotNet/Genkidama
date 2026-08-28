function Enterprise_Adapter_Pattern return Boolean is
   type Legacy_Record is record
      Code  : Integer;
      Cents : Integer;
   end record;
   type Canonical_Record is record
      Id     : Integer;
      Amount : Long_Float;
   end record;

   function Adapt (Legacy : Legacy_Record) return Canonical_Record is
   begin
      return (Id => Legacy.Code, Amount => Long_Float (Legacy.Cents) / 100.0);
   end Adapt;

   Canonical : constant Canonical_Record := Adapt ((Code => 17, Cents => 1250));
begin
   return Canonical.Id = 17 and then abs (Canonical.Amount - 12.5) < 1.0E-9;
end Enterprise_Adapter_Pattern;
