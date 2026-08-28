module command_example
  implicit none
contains
  logical function run()
    integer :: balance
    balance = 100
    call deposit(balance, 50)
    call withdraw(balance, 20)
    run = balance == 130
  contains
    subroutine deposit(value, amount)
      integer, intent(inout) :: value
      integer, intent(in) :: amount
      value = value + amount
    end subroutine
    subroutine withdraw(value, amount)
      integer, intent(inout) :: value
      integer, intent(in) :: amount
      value = value - amount
    end subroutine
  end function
end module
