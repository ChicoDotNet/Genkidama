module service_locator_example
  implicit none
contains
  logical function run()
    run = trim(locate('email','a@example.test')) == 'email>a@example.test' .and. &
          trim(locate('audit','created')) == 'audit>created'
  contains
    function locate(service, value) result(text)
      character(len=*), intent(in) :: service, value
      character(len=96) :: text
      text = trim(service)//'>'//trim(value)
    end function
  end function
end module
