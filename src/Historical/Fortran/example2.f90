module database_module
  implicit none
  type, abstract :: database
    contains
      procedure(connect), deferred :: connect
      procedure(query), deferred :: query
  end type database

  abstract interface
    subroutine connect(self)
      import :: database
      class(database), intent(inout) :: self
    end subroutine connect

    subroutine query(self)
      import :: database
      class(database), intent(inout) :: self
    end subroutine query
  end interface

  type, extends(database) :: postgres
  contains
    procedure :: connect => connect_postgres
    procedure :: query => query_postgres
  end type postgres

  type, extends(database) :: mysql
  contains
    procedure :: connect => connect_mysql
    procedure :: query => query_mysql
  end type mysql

contains

  subroutine connect_postgres(self)
    class(postgres), intent(inout) :: self
    print *, 'Connecting to PostgreSQL'
  end subroutine connect_postgres

  subroutine query_postgres(self)
    class(postgres), intent(inout) :: self
    print *, 'Querying PostgreSQL'
  end subroutine query_postgres

  subroutine connect_mysql(self)
    class(mysql), intent(inout) :: self
    print *, 'Connecting to MySQL'
  end subroutine connect_mysql

  subroutine query_mysql(self)
    class(mysql), intent(inout) :: self
    print *, 'Querying MySQL'
  end subroutine query_mysql

end module database_module

program example2
  use database_module
  implicit none
  class(database), pointer :: db

  db => new_database('postgresql')
  call db%connect()
  call db%query()

  db => new_database('mysql')
  call db%connect()
  call db%query()

contains

  function new_database(db_type) result(db)
    character(len=*), intent(in) :: db_type
    class(database), pointer :: db

    if (db_type == 'postgresql') then
      allocate(postgres::db)
    else if (db_type == 'mysql') then
      allocate(mysql::db)
    end if
  end function new_database

end program example2
