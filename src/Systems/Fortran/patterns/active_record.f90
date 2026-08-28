module active_record_example
  implicit none
  type :: record
    integer :: id
    character(len=16) :: name
  end type
contains
  logical function run()
    type(record) :: row
    row = record(7,'Ada')
    run = row%id == 7 .and. trim(row%name) == 'Ada'
  end function
end module
