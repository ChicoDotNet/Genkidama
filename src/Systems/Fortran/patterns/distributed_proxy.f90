module distributed_proxy_example
  implicit none
contains
  logical function run()
    run = proxy('sku-1') == 7
  contains
    integer function proxy(sku)
      character(len=*), intent(in) :: sku
      proxy = remote(sku)
    end function
    integer function remote(sku)
      character(len=*), intent(in) :: sku
      if (sku == 'sku-1') then; remote = 7; else; remote = 0; end if
    end function
  end function
end module
