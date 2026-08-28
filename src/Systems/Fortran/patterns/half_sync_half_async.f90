module half_sync_half_async_example
  implicit none
contains
  logical function run()
    character(len=8) :: jobs(3)
    character(len=64) :: result
    integer :: i
    jobs = ['job-1   ','job-2   ','job-3   ']
    result = ''
    do i = 1, size(jobs)
      if (i > 1) result = trim(result)//'>'
      result = trim(result)//'done:'//trim(jobs(i))
    end do
    run = trim(result) == 'done:job-1>done:job-2>done:job-3'
  end function
end module
