module microkernel_example
  implicit none
contains
  logical function run()
    run = apply_plugin('double', 4) == 8 .and. apply_plugin('square', 4) == 16
  contains
    integer function apply_plugin(name, value)
      character(len=*), intent(in) :: name
      integer, intent(in) :: value
      select case (name)
      case ('double'); apply_plugin = value * 2
      case ('square'); apply_plugin = value * value
      case default; apply_plugin = value
      end select
    end function
  end function
end module
