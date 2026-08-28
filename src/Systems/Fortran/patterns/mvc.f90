module mvc_example
  implicit none
contains
  logical function run()
    integer :: model
    character(len=16) :: before, after
    model = 0
    before = render(model)
    call increment(model)
    after = render(model)
    run = trim(before) == 'count=0' .and. trim(after) == 'count=1'
  contains
    subroutine increment(value)
      integer, intent(inout) :: value
      value = value + 1
    end subroutine
    function render(value) result(text)
      integer, intent(in) :: value
      character(len=16) :: text
      write(text,'("count=",I0)') value
    end function
  end function
end module
