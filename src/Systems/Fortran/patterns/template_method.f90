module template_method_example
  implicit none
contains
  logical function run()
    run = trim(pipeline('read-csv', 'normalize')) == 'read-csv>normalize>publish'
  contains
    function pipeline(read_step, transform) result(text)
      character(len=*), intent(in) :: read_step, transform
      character(len=96) :: text
      text = trim(read_step)//'>'//trim(transform)//'>publish'
    end function
  end function
end module
