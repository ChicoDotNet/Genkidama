module dependency_injection_example
  implicit none
  abstract interface
    function clock_fn() result(text)
      character(len=5) :: text
    end function
  end interface
contains
  logical function run()
    run = trim(service(fixed_clock)) == 'at:10:00'
  contains
    function fixed_clock() result(text)
      character(len=5) :: text
      text = '10:00'
    end function
    function service(clock) result(text)
      procedure(clock_fn) :: clock
      character(len=16) :: text
      text = 'at:'//clock()
    end function
  end function
end module
