module memento_example
  implicit none
contains
  logical function run()
    character(len=16) :: state, snapshot
    state = 'draft'
    snapshot = state
    state = 'published'
    if (trim(state) /= 'published') then
      run = .false.; return
    end if
    state = snapshot
    run = trim(state) == 'draft'
  end function
end module
