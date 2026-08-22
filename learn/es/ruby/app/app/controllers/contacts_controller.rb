class ContactsController < ApplicationController
  def index
    @contacts = Contact.order(:name)
  end

  def new
    @contact = Contact.new(status: "lead")
  end

  def create
    @contact = Contact.new(contact_params)
    if @contact.save
      redirect_to contacts_path, notice: "Contacto creado."
    else
      render :new, status: :unprocessable_entity
    end
  end

  private

  def contact_params
    params.require(:contact).permit(:name, :email, :company, :status)
  end
end
