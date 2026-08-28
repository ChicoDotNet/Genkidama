module peer_to_peer_example
  implicit none
contains
  logical function run()
    character(len=96) :: inbox
    inbox = ''
    call send_block('peer-a','peer-b','block-42',inbox)
    call send_block('peer-a','peer-c','block-42',inbox)
    run = trim(inbox) == 'peer-a>peer-b:block-42>peer-a>peer-c:block-42'
  contains
    subroutine send_block(source, target, data, inbox_value)
      character(len=*), intent(in) :: source, target, data
      character(len=*), intent(inout) :: inbox_value
      if (len_trim(inbox_value) > 0) inbox_value = trim(inbox_value)//'>'
      inbox_value = trim(inbox_value)//trim(source)//'>'//trim(target)//':'//trim(data)
    end subroutine
  end function
end module
