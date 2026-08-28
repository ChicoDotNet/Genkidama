module enterprise_facade_example
  implicit none
contains
  logical function run()
    character(len=96) :: result
    result = onboard(77)
    run = trim(result) == 'crm:create:77>billing:open:77'
  contains
    function onboard(id) result(text)
      integer, intent(in) :: id
      character(len=96) :: text
      character(len=16) :: value
      write(value,'(I0)') id
      text = 'crm:create:'//trim(value)//'>billing:open:'//trim(value)
    end function
  end function
end module
