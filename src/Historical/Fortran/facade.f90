module facade_services
  implicit none
contains
  function authenticate(user) result(text)
    character(len=*), intent(in) :: user
    character(len=:), allocatable :: text
    text = 'auth(' // trim(user) // ')'
  end function authenticate

  function reserve(sku) result(text)
    character(len=*), intent(in) :: sku
    character(len=:), allocatable :: text
    text = 'reserve(' // trim(sku) // ')'
  end function reserve

  function charge(cents) result(text)
    integer, intent(in) :: cents
    character(len=32) :: amount
    character(len=:), allocatable :: text
    write(amount, '(I0)') cents
    text = 'charge(' // trim(amount) // ')'
  end function charge

  function checkout(user, sku, cents) result(text)
    character(len=*), intent(in) :: user, sku
    integer, intent(in) :: cents
    character(len=:), allocatable :: text
    text = 'checkout=' // authenticate(user) // '>' // reserve(sku) // '>' // charge(cents)
  end function checkout
end module facade_services

program facade_example
  use facade_services
  implicit none
  print '(A)', checkout('alice', 'SKU-42', 499)
end program facade_example
