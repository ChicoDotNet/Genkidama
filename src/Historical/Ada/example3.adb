with Ada.Text_IO; use Ada.Text_IO;

procedure Example3 is
   type Report is abstract tagged null record;
   procedure Generate (R : in out Report) is abstract;

   type PDFReport is new Report with null record;
   procedure Generate (R : in out PDFReport);
   type HTMLReport is new Report with null record;
   procedure Generate (R : in out HTMLReport);

   type Report_Factory is abstract tagged null record;
   function Create_Report (Factory : in Report_Factory) return Report'Class is abstract;

   type PDFReport_Factory is new Report_Factory with null record;
   function Create_Report (Factory : in PDFReport_Factory) return Report'Class;
   type HTMLReport_Factory is new Report_Factory with null record;
   function Create_Report (Factory : in HTMLReport_Factory) return Report'Class;

   procedure Generate (R : in out PDFReport) is
   begin
      Put_Line ("Generating PDF report");
   end Generate;

   procedure Generate (R : in out HTMLReport) is
   begin
      Put_Line ("Generating HTML report");
   end Generate;

   function Create_Report (Factory : in PDFReport_Factory) return Report'Class is
   begin
      return new PDFReport;
   end Create_Report;

   function Create_Report (Factory : in HTMLReport_Factory) return Report'Class is
   begin
      return new HTMLReport;
   end Create_Report;

   procedure Use_Factory (Factory : in Report_Factory) is
      R : Report'Class := Factory.Create_Report;
   begin
      R.Generate;
   end Use_Factory;

begin
   declare
      PDF_Factory : PDFReport_Factory;
      HTML_Factory : HTMLReport_Factory;
   begin
      Use_Factory (PDF_Factory);
      Use_Factory (HTML_Factory);
   end;
end Example3;
