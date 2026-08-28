module presentation_abstraction_control_example
  implicit none
contains
  logical function run()
    run = trim(render('child',42)) == 'child:view=42' .and. trim(render('root',42)) == 'root:view=42'
  contains
    function render(name, value) result(text)
      character(len=*), intent(in) :: name
      integer, intent(in) :: value
      character(len=64) :: text
      character(len=16) :: number
      write(number,'(I0)') value
      text = trim(name)//':view='//trim(number)
    end function
  end function
end module
