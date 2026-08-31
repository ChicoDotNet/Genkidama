module mediator_example
  implicit none

  abstract interface
    subroutine receiver(sender, message, log)
      character(len=*), intent(in) :: sender, message
      character(len=*), intent(inout) :: log
    end subroutine receiver
  end interface

  type :: colleague
    character(len=16) :: name = ''
    procedure(receiver), pointer, nopass :: receive => null()
  end type colleague

contains
  subroutine inventory_receive(sender, message, log)
    character(len=*), intent(in) :: sender, message
    character(len=*), intent(inout) :: log
    if (sender /= 'payment' .or. message /= 'paid') error stop 'unexpected inventory message'
    log = 'inventory<-payment:paid'
  end subroutine inventory_receive

  subroutine payment_receive(sender, message, log)
    character(len=*), intent(in) :: sender, message
    character(len=*), intent(inout) :: log
    if (sender /= 'inventory' .or. message /= 'reserved') error stop 'unexpected payment message'
    log = trim(log)//'>payment<-inventory:reserved'
  end subroutine payment_receive

  logical function send(routes, sender, recipient, message, log)
    type(colleague), intent(in) :: routes(:)
    character(len=*), intent(in) :: sender, recipient, message
    character(len=*), intent(inout) :: log
    integer :: i

    send = .false.
    do i = 1, size(routes)
      if (trim(routes(i)%name) == recipient) then
        call routes(i)%receive(sender, message, log)
        send = .true.
        return
      end if
    end do
  end function send

  logical function run()
    type(colleague) :: routes(2)
    character(len=96) :: events

    events = ''
    routes(1)%name = 'inventory'
    routes(1)%receive => inventory_receive
    routes(2)%name = 'payment'
    routes(2)%receive => payment_receive

    run = send(routes, 'payment', 'inventory', 'paid', events)
    run = run .and. send(routes, 'inventory', 'payment', 'reserved', events)
    run = run .and. trim(events) == 'inventory<-payment:paid>payment<-inventory:reserved'
    run = run .and. .not. send(routes, 'payment', 'unknown', 'ignored', events)
  end function run
end module mediator_example
