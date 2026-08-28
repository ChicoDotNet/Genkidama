module unit_of_work_example
  implicit none
contains
  logical function run()
    integer :: pending(2), store(2)
    pending = [2,3]; store = 0
    call commit(pending, store)
    run = all(store == [2,3]) .and. all(pending == 0)
  contains
    subroutine commit(pending_items, store_items)
      integer, intent(inout) :: pending_items(:)
      integer, intent(out) :: store_items(:)
      store_items = pending_items
      pending_items = 0
    end subroutine
  end function
end module
