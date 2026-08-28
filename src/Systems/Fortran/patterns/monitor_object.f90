module monitor_object_example
  implicit none
  type :: counter
    integer :: value = 0
  contains
    procedure :: add => counter_add
  end type
contains
  subroutine counter_add(self, amount)
    class(counter), intent(inout) :: self
    integer, intent(in) :: amount
    self%value = self%value + amount
  end subroutine
  logical function run()
    type(counter) :: item
    call item%add(2); call item%add(3)
    run = item%value == 5
  end function
end module
