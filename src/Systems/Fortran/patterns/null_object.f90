module null_object_example
  implicit none
contains
  logical function run()
    run = len_trim(log_message(.false., 'x')) == 0 .and. trim(log_message(.true., 'x')) == 'log:x'
  contains
    function log_message(enabled, message) result(text)
      logical, intent(in) :: enabled
      character(len=*), intent(in) :: message
      character(len=32) :: text
      if (enabled) then; text = 'log:'//trim(message); else; text = ''; end if
    end function
  end function
end module
