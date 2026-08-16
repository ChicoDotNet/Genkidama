module builder_pattern
    implicit none
    private
    public :: report_builder, text_report, html_report, build_availability_report

    integer, parameter :: text_report = 1, html_report = 2

    type :: report_builder
        integer :: format
    contains
        procedure :: reset
        procedure :: add_title
        procedure :: add_section
    end type report_builder

contains

    subroutine reset(self)
        class(report_builder), intent(inout) :: self
    end subroutine reset

    subroutine add_title(self, title)
        class(report_builder), intent(in) :: self
        character(len=*), intent(in) :: title
        if (self%format == text_report) then
            print '(A)', '# ' // trim(title)
        else
            print '(A)', '<h1>' // trim(title) // '</h1>'
        end if
    end subroutine add_title

    subroutine add_section(self, heading, body)
        class(report_builder), intent(in) :: self
        character(len=*), intent(in) :: heading, body
        if (self%format == text_report) then
            print '(A)', '## ' // trim(heading)
            print '(A)', trim(body)
        else
            print '(A)', '<h2>' // trim(heading) // '</h2><p>' // trim(body) // '</p>'
        end if
    end subroutine add_section

    subroutine build_availability_report(builder)
        type(report_builder), intent(inout) :: builder
        call builder%reset()
        call builder%add_title('Service status')
        call builder%add_section('Availability', '99.95%')
    end subroutine build_availability_report

end module builder_pattern

program builder
    use builder_pattern
    implicit none
    type(report_builder) :: text, html

    text%format = text_report
    html%format = html_report
    call build_availability_report(text)
    print '(A)', '---'
    call build_availability_report(html)
end program builder
