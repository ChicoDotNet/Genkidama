module document_view_example
  implicit none
contains
  logical function run()
    character(len=64) :: editor, summary
    call render_views('Final',120,editor,summary)
    run = trim(editor) == 'editor:Final:120' .and. trim(summary) == 'summary:Final'
  contains
    subroutine render_views(title, words, editor_text, summary_text)
      character(len=*), intent(in) :: title
      integer, intent(in) :: words
      character(len=*), intent(out) :: editor_text, summary_text
      character(len=16) :: number
      write(number,'(I0)') words
      editor_text = 'editor:'//trim(title)//':'//trim(number)
      summary_text = 'summary:'//trim(title)
    end subroutine
  end function
end module
