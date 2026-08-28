module publish_subscribe_example
  implicit none
contains
  logical function run()
    character(len=64) :: log
    call publish(51, log)
    run = trim(log) == 'warehouse:51>analytics:51'
  contains
    subroutine publish(id, target)
      integer, intent(in) :: id
      character(len=*), intent(out) :: target
      character(len=16) :: text
      write(text,'(I0)') id
      target = 'warehouse:'//trim(text)//'>analytics:'//trim(text)
    end subroutine
  end function
end module
