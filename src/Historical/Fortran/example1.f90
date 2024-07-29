module ui_factory
  implicit none
  type, abstract :: button
    contains
      procedure(render_button), deferred :: render
  end type button

  type, abstract :: checkbox
    contains
      procedure(render_checkbox), deferred :: render
  end type checkbox

  abstract interface
    subroutine render_button(self)
      import :: button
      class(button), intent(inout) :: self
    end subroutine render_button

    subroutine render_checkbox(self)
      import :: checkbox
      class(checkbox), intent(inout) :: self
    end subroutine render_checkbox
  end interface
end module ui_factory

module dark_theme
  use ui_factory
  implicit none
  type, extends(button) :: dark_button
  contains
    procedure :: render => render_dark_button
  end type dark_button

  type, extends(checkbox) :: dark_checkbox
  contains
    procedure :: render => render_dark_checkbox
  end type dark_checkbox

  contains
    subroutine render_dark_button(self)
      class(dark_button), intent(inout) :: self
      print *, 'Dark Button'
    end subroutine render_dark_button

    subroutine render_dark_checkbox(self)
      class(dark_checkbox), intent(inout) :: self
      print *, 'Dark Checkbox'
    end subroutine render_dark_checkbox
end module dark_theme

module light_theme
  use ui_factory
  implicit none
  type, extends(button) :: light_button
  contains
    procedure :: render => render_light_button
  end type light_button

  type, extends(checkbox) :: light_checkbox
  contains
    procedure :: render => render_light_checkbox
  end type light_checkbox

  contains
    subroutine render_light_button(self)
      class(light_button), intent(inout) :: self
      print *, 'Light Button'
    end subroutine render_light_button

    subroutine render_light_checkbox(self)
      class(light_checkbox), intent(inout) :: self
      print *, 'Light Checkbox'
    end subroutine render_light_checkbox
end module light_theme

program example1
  use ui_factory
  use dark_theme
  use light_theme
  implicit none
  type(button) :: btn
  type(checkbox) :: chk

  btn = dark_button()
  call btn%render()

  chk = light_checkbox()
  call chk%render()
end program example1
