module repository_example
  implicit none
contains
  logical function run()
    integer :: ids(2)
    character(len=8) :: names(2), found
    ids = [1,2]; names = ['Ada     ','Grace   ']
    found = find_name(ids,names,2)
    run = trim(found) == 'Grace'
  contains
    function find_name(ids_value,names_value,wanted) result(name)
      integer, intent(in) :: ids_value(:), wanted
      character(len=*), intent(in) :: names_value(:)
      character(len=8) :: name
      integer :: i
      name = ''
      do i=1,size(ids_value); if(ids_value(i)==wanted) name=names_value(i); end do
    end function
  end function
end module
