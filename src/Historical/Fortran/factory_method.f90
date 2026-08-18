program factory_method
  implicit none

  abstract interface
    integer function factory_method_proc()
    end function factory_method_proc
  end interface

  call use_database(create_postgres)
  call use_database(create_mysql)

contains

  integer function create_postgres()
    create_postgres = 1
  end function create_postgres

  integer function create_mysql()
    create_mysql = 2
  end function create_mysql

  subroutine use_database(create_database)
    procedure(factory_method_proc) :: create_database
    integer :: database

    database = create_database()
    select case (database)
    case (1)
      print '(A)', 'PostgreSQL connect'
      print '(A)', 'PostgreSQL query'
    case (2)
      print '(A)', 'MySQL connect'
      print '(A)', 'MySQL query'
    case default
      error stop 'unknown database'
    end select
  end subroutine use_database

end program factory_method
