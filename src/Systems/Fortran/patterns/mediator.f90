module mediator_example
  implicit none
contains
  logical function run()
    character(len=64) :: events
    events = ''
    call notify('button', 'click', events)
    call notify('panel', 'loaded', events)
    run = trim(events) == 'panel.refresh>button.enable'
  contains
    subroutine notify(sender, event, log)
      character(len=*), intent(in) :: sender, event
      character(len=*), intent(inout) :: log
      if (sender == 'button' .and. event == 'click') log = 'panel.refresh'
      if (sender == 'panel' .and. event == 'loaded') log = trim(log)//'>button.enable'
    end subroutine
  end function
end module
