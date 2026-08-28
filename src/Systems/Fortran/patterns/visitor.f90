module visitor_example
  implicit none
contains
  logical function run()
    real(8) :: total, expected
    total = circle_area(2.0d0) + rectangle_area(3.0d0, 4.0d0)
    expected = 4.0d0 * acos(-1.0d0) + 12.0d0
    run = abs(total - expected) < 1.0d-9
  contains
    real(8) function circle_area(radius)
      real(8), intent(in) :: radius
      circle_area = acos(-1.0d0) * radius * radius
    end function
    real(8) function rectangle_area(width, height)
      real(8), intent(in) :: width, height
      rectangle_area = width * height
    end function
  end function
end module
