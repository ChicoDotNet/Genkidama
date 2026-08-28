module data_mapper_example
  implicit none
  type :: person
    integer :: id
    character(len=16) :: name
  end type
contains
  logical function run()
    type(person) :: entity, restored
    character(len=16) :: key
    entity = person(8,'Grace')
    write(key,'("person:",I0)') entity%id
    read(key(8:),*) restored%id
    restored%name = entity%name
    run = trim(key) == 'person:8' .and. restored%id == entity%id .and. trim(restored%name) == 'Grace'
  end function
end module
