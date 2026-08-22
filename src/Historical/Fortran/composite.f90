program composite
  implicit none

  integer, parameter :: node_count = 5
  logical :: is_file(node_count)
  integer :: bytes(node_count)
  integer :: left_child(node_count)
  integer :: right_child(node_count)

  is_file = [.true., .true., .true., .false., .false.]
  bytes = [2, 3, 5, 0, 0]
  left_child = [0, 0, 0, 2, 1]
  right_child = [0, 0, 0, 3, 4]

  print '(A,I0)', 'leaf=', node_size(1)
  print '(A,I0)', 'docs=', node_size(4)
  print '(A,I0)', 'root=', node_size(5)

contains

  recursive function node_size(node_id) result(total)
    integer, intent(in) :: node_id
    integer :: total

    if (is_file(node_id)) then
      total = bytes(node_id)
      return
    end if

    total = node_size(left_child(node_id)) + node_size(right_child(node_id))
  end function node_size

end program composite
