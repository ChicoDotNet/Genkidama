module state_example
  implicit none
contains
  logical function run()
    integer :: state
    state = 0
    state = transition(state, 'unlock')
    state = transition(state, 'lock')
    run = state == 0
  contains
    integer function transition(current, action)
      integer, intent(in) :: current
      character(len=*), intent(in) :: action
      transition = current
      if (current == 0 .and. action == 'unlock') transition = 1
      if (current == 1 .and. action == 'lock') transition = 0
    end function
  end function
end module
