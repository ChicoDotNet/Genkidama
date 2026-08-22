module temperature_adapter
  implicit none

  type :: legacy_fahrenheit_sensor
    integer :: fahrenheit = 86
  contains
    procedure :: read_fahrenheit
  end type legacy_fahrenheit_sensor

  type :: fahrenheit_sensor_adapter
    type(legacy_fahrenheit_sensor) :: adaptee
  contains
    procedure :: read_celsius
  end type fahrenheit_sensor_adapter

contains

  integer function read_fahrenheit(self)
    class(legacy_fahrenheit_sensor), intent(in) :: self
    read_fahrenheit = self%fahrenheit
  end function read_fahrenheit

  integer function read_celsius(self)
    class(fahrenheit_sensor_adapter), intent(in) :: self
    integer :: fahrenheit
    fahrenheit = self%adaptee%read_fahrenheit()
    read_celsius = ((fahrenheit - 32) * 5) / 9
  end function read_celsius

end module temperature_adapter

program adapter_demo
  use temperature_adapter
  implicit none

  type(legacy_fahrenheit_sensor) :: legacy
  type(fahrenheit_sensor_adapter) :: adapter

  adapter%adaptee = legacy
  write (*, '(A,I0,A)') 'legacy=', legacy%read_fahrenheit(), 'F'
  write (*, '(A,I0,A)') 'adapted=', adapter%read_celsius(), 'C'
end program adapter_demo
