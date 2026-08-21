module registry_module
  implicit none
  type :: registry_state
    integer :: count = 0
  end type registry_state
  type(registry_state), target, save :: shared
contains
  function instance() result(value)
    type(registry_state), pointer :: value
    value => shared
  end function instance
end module registry_module

program singleton
  use registry_module
  implicit none
  type(registry_state), pointer :: first, second

  first => instance()
  second => instance()
  first%count = first%count + 1

  if (associated(first, second)) then
    print '(A)', 'same=true'
  else
    print '(A)', 'same=false'
  end if
  print '(A,I0)', 'count=', second%count
end program singleton
