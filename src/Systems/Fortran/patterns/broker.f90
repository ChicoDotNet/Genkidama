module broker_example
  implicit none
contains
  logical function run()
    run = trim(route('inventory','sku-1')) == 'inventory:sku-1=7' .and. &
          trim(route('customer','17')) == 'customer:17=active'
  contains
    function route(service, key) result(text)
      character(len=*), intent(in) :: service, key
      character(len=96) :: text
      if (service == 'inventory') then
        text = 'inventory:'//trim(key)//'=7'
      else
        text = 'customer:'//trim(key)//'=active'
      end if
    end function
  end function
end module
