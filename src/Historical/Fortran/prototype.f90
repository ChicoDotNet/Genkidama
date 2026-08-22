program prototype_example
  implicit none

  type :: service_profile
     character(len=32) :: name = ''
     character(len=32), allocatable :: features(:)
  end type service_profile

  type(service_profile) :: original, canary

  original%name = 'orders'
  allocate(original%features(1))
  original%features(1) = 'metrics'

  canary = clone_profile(original)
  canary%name = 'orders-canary'
  call add_feature(canary, 'tracing')

  print '(A)', 'original=' // describe(original)
  print '(A)', 'clone=' // describe(canary)

contains

  function clone_profile(source) result(copy)
    type(service_profile), intent(in) :: source
    type(service_profile) :: copy

    copy%name = source%name
    if (allocated(source%features)) then
       allocate(copy%features(size(source%features)))
       copy%features = source%features
    end if
  end function clone_profile

  subroutine add_feature(profile, feature)
    type(service_profile), intent(inout) :: profile
    character(len=*), intent(in) :: feature
    character(len=32), allocatable :: expanded(:)
    integer :: count

    count = size(profile%features)
    allocate(expanded(count + 1))
    expanded(1:count) = profile%features
    expanded(count + 1) = feature
    call move_alloc(expanded, profile%features)
  end subroutine add_feature

  function describe(profile) result(text)
    type(service_profile), intent(in) :: profile
    character(len=256) :: text
    integer :: i

    text = trim(profile%name) // ':'
    do i = 1, size(profile%features)
       if (i == 1) then
          text = trim(text) // ' ' // trim(profile%features(i))
       else
          text = trim(text) // ',' // trim(profile%features(i))
       end if
    end do
  end function describe

end program prototype_example
