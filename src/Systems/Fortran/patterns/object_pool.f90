module object_pool_example
  implicit none
contains
  logical function run()
    integer :: pool(2), borrowed
    pool = [1,2]
    borrowed = pool(2); pool(2) = 0; pool(2) = borrowed
    run = all(pool == [1,2]) .and. borrowed == 2
  end function
end module
