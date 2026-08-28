module strategy_example
  implicit none
contains
  logical function run()
    run = price(100, 0) == 100 .and. price(100, 1) == 80
  contains
    integer function price(value, strategy)
      integer, intent(in) :: value, strategy
      if (strategy == 1) then
        price = value * 80 / 100
      else
        price = value
      end if
    end function
  end function
end module
