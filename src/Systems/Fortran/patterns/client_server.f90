module client_server_example
  implicit none
contains
  logical function run()
    character(len=32) :: response
    response = server('sku-1')
    run = trim(response) == '200:stock=7'
  contains
    function server(key) result(text)
      character(len=*), intent(in) :: key
      character(len=32) :: text
      if (key == 'sku-1') then; text = '200:stock=7'; else; text = '404:missing'; end if
    end function
  end function
end module
