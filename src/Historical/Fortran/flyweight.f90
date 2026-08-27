program flyweight
  implicit none

  type :: text_style
    character(len=5) :: font
    integer :: size
    character(len=4) :: color
    logical :: used
  end type text_style

  type(text_style) :: pool(2)
  integer :: red1, red2, blue, styles

  pool%used = .false.
  red1 = get_style('Inter', 12, 'red ')
  red2 = get_style('Inter', 12, 'red ')
  blue = get_style('Inter', 12, 'blue')

  if (pool(blue)%color /= 'blue') error stop 'blue style missing'
  styles = count(pool%used)

  write (*,'(A,I0,A,A,A)') 'styles=', styles, ';shared=', &
       merge('true ', 'false', red1 == red2), ';text=ABC'

contains

  integer function get_style(font, font_size, color) result(id)
    character(len=*), intent(in) :: font, color
    integer, intent(in) :: font_size
    integer :: i

    do i = 1, size(pool)
      if (pool(i)%used .and. pool(i)%font == font .and. &
          pool(i)%size == font_size .and. pool(i)%color == color) then
        id = i
        return
      end if
    end do

    do i = 1, size(pool)
      if (.not. pool(i)%used) then
        pool(i) = text_style(font, font_size, color, .true.)
        id = i
        return
      end if
    end do

    error stop 'style pool exhausted'
  end function get_style
end program flyweight
