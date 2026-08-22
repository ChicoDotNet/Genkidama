program decorator
  implicit none
  abstract interface
     function render_fn() result(text)
       character(len=:), allocatable :: text
     end function render_fn
  end interface

  print '(A)', 'base=' // plain()
  print '(A)', 'audit=' // audit(plain)
  print '(A)', 'encrypted=' // encrypt(plain)
  print '(A)', 'stacked=' // audit(encrypted_plain)
contains
  function plain() result(text)
    character(len=:), allocatable :: text
    text = 'alert'
  end function plain

  function audit(inner) result(text)
    procedure(render_fn) :: inner
    character(len=:), allocatable :: text
    text = 'audit(' // inner() // ')'
  end function audit

  function encrypt(inner) result(text)
    procedure(render_fn) :: inner
    character(len=:), allocatable :: text
    text = 'enc(' // inner() // ')'
  end function encrypt

  function encrypted_plain() result(text)
    character(len=:), allocatable :: text
    text = encrypt(plain)
  end function encrypted_plain
end program decorator
