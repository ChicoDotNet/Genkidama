module proxy_example
  implicit none

  type :: remote_document_store
    integer :: fetches = 0
  contains
    procedure :: get_document => remote_get_document
  end type

  type :: document_store_proxy
    type(remote_document_store), allocatable :: backend
    logical :: has_cache = .false.
    integer :: cached_id = 0
    character(len=32) :: cached_value = ''
  contains
    procedure :: get_document => proxy_get_document
    procedure :: backend_count
    procedure :: fetch_count
  end type

contains

  function remote_get_document(self, id) result(value)
    class(remote_document_store), intent(inout) :: self
    integer, intent(in) :: id
    character(len=32) :: value
    self%fetches = self%fetches + 1
    write(value, '(A,I0,A)') 'doc(', id, ')'
  end function

  function proxy_get_document(self, id) result(value)
    class(document_store_proxy), intent(inout) :: self
    integer, intent(in) :: id
    character(len=32) :: value

    if (self%has_cache .and. self%cached_id == id) then
      value = self%cached_value
      return
    end if

    if (.not. allocated(self%backend)) allocate(self%backend)
    self%cached_id = id
    self%cached_value = self%backend%get_document(id)
    self%has_cache = .true.
    value = self%cached_value
  end function

  integer function backend_count(self)
    class(document_store_proxy), intent(in) :: self
    if (allocated(self%backend)) then
      backend_count = 1
    else
      backend_count = 0
    end if
  end function

  integer function fetch_count(self)
    class(document_store_proxy), intent(in) :: self
    if (allocated(self%backend)) then
      fetch_count = self%backend%fetches
    else
      fetch_count = 0
    end if
  end function

end module

program main
  use proxy_example
  implicit none
  type(document_store_proxy) :: store
  character(len=32) :: first_value, second_value

  first_value = store%get_document(42)
  second_value = store%get_document(42)
  write(*, '(A,I0,A,I0,A,A,A,A)') 'backend=', store%backend_count(), &
    ';fetches=', store%fetch_count(), ';first=', trim(first_value), ';second=', trim(second_value)
end program
