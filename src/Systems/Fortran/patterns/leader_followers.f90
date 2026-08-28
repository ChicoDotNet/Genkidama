module leader_followers_example
  implicit none
contains
  logical function run()
    character(len=8) :: workers(3)
    character(len=1) :: events(3)
    character(len=64) :: handled
    integer :: i
    workers = ['worker-1','worker-2','worker-3']; events = ['a','b','c']; handled = ''
    do i = 1, 3
      if (i > 1) handled = trim(handled)//'>'
      handled = trim(handled)//trim(workers(i))//':'//events(i)
    end do
    run = trim(handled) == 'worker-1:a>worker-2:b>worker-3:c'
  end function
end module
