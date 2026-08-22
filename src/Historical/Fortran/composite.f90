program composite
  implicit none

  type :: node
    logical :: is_file = .false.
    integer :: bytes = 0
    type(node), allocatable :: children(:)
  end type node

  type(node) :: readme, docs, root

  readme = file_node(2)
  docs = folder_node([file_node(3), file_node(5)])
  root = folder_node([readme, docs])

  print '(A,I0)', 'leaf=', node_size(readme)
  print '(A,I0)', 'docs=', node_size(docs)
  print '(A,I0)', 'root=', node_size(root)

contains

  function file_node(bytes) result(item)
    integer, intent(in) :: bytes
    type(node) :: item
    item%is_file = .true.
    item%bytes = bytes
  end function file_node

  function folder_node(children) result(item)
    type(node), intent(in) :: children(:)
    type(node) :: item
    item%is_file = .false.
    item%children = children
  end function folder_node

  recursive function node_size(item) result(total)
    type(node), intent(in) :: item
    integer :: total
    integer :: index

    if (item%is_file) then
      total = item%bytes
      return
    end if

    total = 0
    do index = 1, size(item%children)
      total = total + node_size(item%children(index))
    end do
  end function node_size

end program composite
