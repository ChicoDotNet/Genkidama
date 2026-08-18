program factory_method
  implicit none

  abstract interface
    subroutine database_action()
    end subroutine database_action
  end interface

  type :: database_product
    procedure(database_action), pointer, nopass :: connect => null()
    procedure(database_action), pointer, nopass :: query => null()
  end type database_product

  abstract interface
    subroutine factory_method_proc(database)
      import :: database_product
      type(database_product), intent(out) :: database
    end subroutine factory_method_proc
  end interface

  call use_database(create_postgres)
  call use_database(create_mysql)

contains

  subroutine postgres_connect()
    print '(A)', 'PostgreSQL connect'
  end subroutine postgres_connect

  subroutine postgres_query()
    print '(A)', 'PostgreSQL query'
  end subroutine postgres_query

  subroutine mysql_connect()
    print '(A)', 'MySQL connect'
  end subroutine mysql_connect

  subroutine mysql_query()
    print '(A)', 'MySQL query'
  end subroutine mysql_query

  subroutine create_postgres(database)
    type(database_product), intent(out) :: database
    database%connect => postgres_connect
    database%query => postgres_query
  end subroutine create_postgres

  subroutine create_mysql(database)
    type(database_product), intent(out) :: database
    database%connect => mysql_connect
    database%query => mysql_query
  end subroutine create_mysql

  subroutine use_database(create_database)
    procedure(factory_method_proc) :: create_database
    type(database_product) :: database

    call create_database(database)
    call database%connect()
    call database%query()
  end subroutine use_database

end program factory_method
