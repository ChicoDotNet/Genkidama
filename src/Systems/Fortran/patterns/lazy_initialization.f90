module lazy_initialization_example
  implicit none
contains
  logical function run()
    character(len=16) :: cache
    integer :: builds
    cache = ''; builds = 0
    call ensure(cache, builds); call ensure(cache, builds)
    run = trim(cache) == 'ready' .and. builds == 1
  contains
    subroutine ensure(value, count)
      character(len=*), intent(inout) :: value
      integer, intent(inout) :: count
      if (len_trim(value) == 0) then; value = 'ready'; count = count + 1; end if
    end subroutine
  end function
end module
