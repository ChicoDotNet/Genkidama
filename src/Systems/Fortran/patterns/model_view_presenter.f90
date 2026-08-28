module model_view_presenter_example
  implicit none
contains
  logical function run()
    integer :: count
    character(len=16) :: text
    count = 0; text = ''
    call present(count, text)
    run = count == 1 .and. trim(text) == 'count=1'
  contains
    subroutine present(value, target)
      integer, intent(inout) :: value
      character(len=*), intent(out) :: target
      value = value + 1
      write(target,'("count=",I0)') value
    end subroutine
  end function
end module
