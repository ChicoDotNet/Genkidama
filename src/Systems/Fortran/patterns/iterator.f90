module iterator_example
  implicit none
contains
  logical function run()
    integer :: values(3), seen(3), i
    values = [10, 20, 30]
    seen = 0
    do i = 1, size(values)
      seen(i) = values(i)
    end do
    run = all(seen == values)
  end function
end module
