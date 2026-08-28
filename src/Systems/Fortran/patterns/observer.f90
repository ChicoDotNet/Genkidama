module observer_example
  implicit none
contains
  logical function run()
    character(len=64) :: log
    log = ''
    call publish(42, log)
    run = trim(log) == 'audit:42>dashboard:42'
  contains
    subroutine publish(id, target)
      integer, intent(in) :: id
      character(len=*), intent(inout) :: target
      character(len=16) :: text
      write(text,'(I0)') id
      target = 'audit:'//trim(text)//'>dashboard:'//trim(text)
    end subroutine
  end function
end module
