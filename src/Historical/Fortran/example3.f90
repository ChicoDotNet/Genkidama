module report_module
  implicit none
  type, abstract :: report
    contains
      procedure(generate), deferred :: generate
  end type report

  abstract interface
    subroutine generate(self)
      import :: report
      class(report), intent(inout) :: self
    end subroutine generate
  end interface

  type, extends(report) :: pdf_report
  contains
    procedure :: generate => generate_pdf_report
  end type pdf_report

  type, extends(report) :: html_report
  contains
    procedure :: generate => generate_html_report
  end type html_report

contains

  subroutine generate_pdf_report(self)
    class(pdf_report), intent(inout) :: self
    print *, 'Generating PDF report'
  end subroutine generate_pdf_report

  subroutine generate_html_report(self)
    class(html_report), intent(inout) :: self
    print *, 'Generating HTML report'
  end subroutine generate_html_report

end module report_module

program example3
  use report_module
  implicit none
  class(report), pointer :: rpt

  rpt => new_report('pdf')
  call rpt%generate()

  rpt => new_report('html')
  call rpt%generate()

contains

  function new_report(report_type) result(rpt)
    character(len=*), intent(in) :: report_type
    class(report), pointer :: rpt

    if (report_type == 'pdf') then
      allocate(pdf_report::rpt)
    else if (report_type == 'html') then
      allocate(html_report::rpt)
    end if
  end function new_report

end program example3
