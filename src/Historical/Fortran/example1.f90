module abstract_factory
    implicit none

    type :: ui_factory
        character(len=5) :: theme
    contains
        procedure :: create_button
        procedure :: create_checkbox
    end type ui_factory

contains

    function dark_factory() result(factory)
        type(ui_factory) :: factory
        factory%theme = 'Dark '
    end function dark_factory

    function light_factory() result(factory)
        type(ui_factory) :: factory
        factory%theme = 'Light'
    end function light_factory

    subroutine create_button(self)
        class(ui_factory), intent(in) :: self
        print '(A,A)', trim(self%theme), ' Button'
    end subroutine create_button

    subroutine create_checkbox(self)
        class(ui_factory), intent(in) :: self
        print '(A,A)', trim(self%theme), ' Checkbox'
    end subroutine create_checkbox

    subroutine render_ui(factory)
        type(ui_factory), intent(in) :: factory
        call factory%create_button()
        call factory%create_checkbox()
    end subroutine render_ui

end module abstract_factory

program example1
    use abstract_factory
    implicit none

    type(ui_factory) :: factory

    factory = dark_factory()
    call render_ui(factory)

    factory = light_factory()
    call render_ui(factory)
end program example1
