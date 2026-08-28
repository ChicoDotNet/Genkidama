module enterprise_bridge_example
  implicit none
contains
  logical function run()
    run = trim(send('kafka','ALERT','disk')) == 'kafka>ALERT:disk' .and. &
          trim(send('queue','REMINDER','backup')) == 'queue>REMINDER:backup'
  contains
    function send(transport, kind, message) result(text)
      character(len=*), intent(in) :: transport, kind, message
      character(len=96) :: text
      text = trim(transport)//'>'//trim(kind)//':'//trim(message)
    end function
  end function
end module
