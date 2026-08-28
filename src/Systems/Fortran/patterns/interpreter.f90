module interpreter_example
  implicit none
contains
  recursive integer function eval(kind, left, right) result(value)
    integer, intent(in) :: kind, left, right
    select case (kind)
    case (0); value = left
    case (1); value = left + right
    case default; value = left * right
    end select
  end function
  logical function run()
    run = eval(1, 7, eval(2, 3, 4)) == 19
  end function
end module
