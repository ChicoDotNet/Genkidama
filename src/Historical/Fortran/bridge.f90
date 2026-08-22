module bridge_types
  implicit none

  abstract interface
    function device_action() result(value)
      character(len=16) :: value
    end function device_action
  end interface

  type :: device
    procedure(device_action), pointer, nopass :: power_on => null()
    procedure(device_action), pointer, nopass :: mute => null()
  end type device
contains
  function tv_on() result(value)
    character(len=16) :: value
    value = 'TV:on'
  end function tv_on

  function tv_mute() result(value)
    character(len=16) :: value
    value = 'TV:muted'
  end function tv_mute

  function radio_on() result(value)
    character(len=16) :: value
    value = 'Radio:on'
  end function radio_on

  function radio_mute() result(value)
    character(len=16) :: value
    value = 'Radio:muted'
  end function radio_mute

  function activate_basic(target) result(value)
    type(device), intent(in) :: target
    character(len=16) :: value
    value = target%power_on()
  end function activate_basic

  function activate_mute(target) result(value)
    type(device), intent(in) :: target
    character(len=16) :: value
    value = target%mute()
  end function activate_mute
end module bridge_types

program bridge
  use bridge_types
  implicit none
  type(device) :: tv, radio

  tv%power_on => tv_on
  tv%mute => tv_mute
  radio%power_on => radio_on
  radio%mute => radio_mute

  print '(A)', 'basic-tv=' // trim(activate_basic(tv))
  print '(A)', 'basic-radio=' // trim(activate_basic(radio))
  print '(A)', 'mute-tv=' // trim(activate_mute(tv))
  print '(A)', 'mute-radio=' // trim(activate_mute(radio))
end program bridge
