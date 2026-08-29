program interpreter_example
  implicit none

  integer, parameter :: NUMBER = 1, PLUS = 2
  type :: token
    integer :: kind
    integer :: value = 0
  end type token

  type(token) :: tokens(5)
  integer :: result

  tokens = [token(NUMBER, 2), token(PLUS, 0), token(NUMBER, 3), token(PLUS, 0), token(NUMBER, 4)]
  result = interpret(tokens)

  if (result /= 9) error stop 'Interpreter expected 9'
  print '(A,I0)', 'interpreter=', result

contains

  integer function interpret(stream) result(value)
    type(token), intent(in) :: stream(:)
    integer :: i

    if (size(stream) == 0 .or. stream(1)%kind /= NUMBER) error stop 'Invalid expression'
    value = stream(1)%value

    i = 2
    do while (i <= size(stream))
      if (i + 1 > size(stream)) error stop 'Invalid expression'
      if (stream(i)%kind /= PLUS .or. stream(i + 1)%kind /= NUMBER) error stop 'Invalid expression'
      value = value + stream(i + 1)%value
      i = i + 2
    end do
  end function interpret
end program interpreter_example
