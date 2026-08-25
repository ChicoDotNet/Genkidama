class NotesController < ApplicationController
  def create
    @contact = Contact.find(params[:contact_id])
    @note = @contact.notes.build(note_params)

    if @note.save
      redirect_to @contact, notice: "Nota agregada."
    else
      @notes = @contact.notes.order(created_at: :desc)
      render "contacts/show", status: :unprocessable_entity
    end
  end

  private

  def note_params
    params.require(:note).permit(:body)
  end
end
