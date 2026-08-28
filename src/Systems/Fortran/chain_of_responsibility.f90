program chain_of_responsibility
  implicit none
  type :: handler
     character(len=10) :: name
     integer :: limit
  end type handler

  type(handler), dimension(3) :: chain
  character(len=32) :: visited
  character(len=10) :: handled
  integer :: amount, i

  chain(1) = handler('faq', 50)
  chain(2) = handler('billing', 500)
  chain(3) = handler('escalation', huge(1))
  amount = 250
  visited = ''
  handled = ''

  do i = 1, size(chain)
     if (len_trim(visited) == 0) then
        visited = trim(chain(i)%name)
     else
        visited = trim(visited) // '>' // trim(chain(i)%name)
     end if

     if (amount <= chain(i)%limit) then
        handled = trim(chain(i)%name)
        exit
     end if
  end do

  if (trim(handled) /= 'billing') error stop 'unexpected handler'
  write(*,'(A,I0,A)') 'visited=' // trim(visited) // ';handled=' // trim(handled) // ';result=refund(', amount, ')'
end program chain_of_responsibility
