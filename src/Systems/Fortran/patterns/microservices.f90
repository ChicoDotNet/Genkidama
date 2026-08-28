module microservices_example
  implicit none
contains
  logical function run()
    integer :: stock
    logical :: reserved
    stock = 7
    reserved = reserve(stock, 2)
    run = reserved .and. stock == 5
  contains
    logical function reserve(stock_value, quantity)
      integer, intent(inout) :: stock_value
      integer, intent(in) :: quantity
      reserve = quantity <= stock_value
      if (reserve) stock_value = stock_value - quantity
    end function
  end function
end module
