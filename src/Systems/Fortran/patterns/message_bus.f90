module message_bus_example
  implicit none
contains
  logical function run()
    character(len=128) :: log
    call publish('order-created', 42, log)
    run = trim(log) == 'audit:order-created:42>billing:order-created:42'
  contains
    subroutine publish(topic, id, target)
      character(len=*), intent(in) :: topic
      integer, intent(in) :: id
      character(len=*), intent(out) :: target
      character(len=16) :: value
      write(value,'(I0)') id
      target = 'audit:'//trim(topic)//':'//trim(value)//'>billing:'//trim(topic)//':'//trim(value)
    end subroutine
  end function
end module
