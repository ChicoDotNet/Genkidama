module mvvm_example
  implicit none
contains
  logical function run()
    integer :: amount
    character(len=16) :: before, after
    amount = 10
    before = format_amount(amount)
    amount = amount + 5
    after = format_amount(amount)
    run = trim(before) == '$10.00' .and. trim(after) == '$15.00'
  contains
    function format_amount(value) result(text)
      integer, intent(in) :: value
      character(len=16) :: text
      write(text,'("$",I0,".00")') value
    end function
  end function
end module
