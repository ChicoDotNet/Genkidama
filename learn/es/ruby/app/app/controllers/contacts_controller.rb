class ContactsController < ApplicationController
  PAGE_SIZE = 20

  before_action :set_contact, only: %i[show edit update]

  def index
    scope = Contact.search(params[:q]).with_status(params[:status]).order(:name)
    @total_count = scope.count
    @total_pages = [(@total_count.to_f / PAGE_SIZE).ceil, 1].max
    @page = [[params.fetch(:page, 1).to_i, 1].max, @total_pages].min
    @contacts = scope.offset((@page - 1) * PAGE_SIZE).limit(PAGE_SIZE)
  end

  def show
    @notes = @contact.notes.order(created_at: :desc)
    @note = Note.new
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

  def edit; end

  def update
    if @contact.update(contact_params)
      redirect_to @contact, notice: "Contacto actualizado."
    else
      render :edit, status: :unprocessable_entity
    end
  end

  def export
    send_data ContactTransfer.export_csv,
      filename: "contactdesk-contacts.csv",
      type: "text/csv; charset=utf-8",
      disposition: "attachment"
  end

  def import
    file = params[:file]
    raise ContactTransfer::ImportError, "Selecciona un archivo CSV." unless file.respond_to?(:read)

    processed = ContactTransfer.import_csv(file)
    redirect_to contacts_path, notice: "Importación completa: #{processed} contacto(s)."
  rescue ContactTransfer::ImportError => e
    redirect_to contacts_path, alert: e.message
  end

  private

  def set_contact
    @contact = Contact.find(params[:id])
  end

  def contact_params
    params.require(:contact).permit(:name, :email, :company, :status)
  end
end
