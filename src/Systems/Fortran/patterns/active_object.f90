module active_object_example
  implicit none
contains
  logical function run()
    integer :: value, queue(2), i
    value = 0; queue = [3, 4]
    do i = 1, size(queue)
      if (i == 1) value = value + queue(i)
      if (i == 2) value = value * queue(i)
    end do
    run = value == 12
  end function
end module
