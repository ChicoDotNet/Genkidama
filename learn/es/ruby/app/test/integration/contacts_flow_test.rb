require "test_helper"
require "tempfile"

class ContactsFlowTest < ActionDispatch::IntegrationTest
  test "lists contacts and creates a valid lead" do
    get contacts_path
    assert_response :success

    assert_difference("Contact.count", 1) do
      post contacts_path, params: { contact: { name: "Luis Vega", email: "luis@example.com", company: "Vega", status: "lead" } }
    end

    assert_redirected_to contacts_path
  end

  test "returns 422 from model validation for invalid contact" do
    post contacts_path, params: { contact: { name: "", email: "bad", status: "lead" } }

    assert_response :unprocessable_entity
    assert_includes response.body, "Revisa los datos"
  end

  test "searches and filters contacts through the index" do
    Contact.create!(name: "Ana Norte", email: "ana@example.com", company: "Norte", status: "lead")
    Contact.create!(name: "Beatriz Sur", email: "bea@example.com", company: "Sur", status: "active")

    get contacts_path, params: { q: "norte", status: "lead" }

    assert_response :success
    assert_includes response.body, "Ana Norte"
    refute_includes response.body, "Beatriz Sur"
  end

  test "paginates contacts with a bounded page size" do
    25.times { |i| Contact.create!(name: format("Contacto %02d", i), email: "page#{i}@example.test", status: "lead") }

    get contacts_path, params: { page: 2 }

    assert_response :success
    assert_includes response.body, "Página 2 de 2"
    assert_includes response.body, "Mostrando 5 de 25 contacto(s)"
  end

  test "updates an existing contact" do
    contact = Contact.create!(name: "Ana", email: "ana@example.com", status: "lead")

    patch contact_path(contact), params: { contact: { name: "Ana Torres", email: contact.email, company: "Norte", status: "active" } }

    assert_redirected_to contact_path(contact)
    contact.reload
    assert_equal "Ana Torres", contact.name
    assert_equal "active", contact.status
  end

  test "adds a note to a contact" do
    contact = Contact.create!(name: "Ana", email: "ana@example.com", status: "lead")

    assert_difference("Note.count", 1) do
      post contact_notes_path(contact), params: { note: { body: "Enviar propuesta" } }
    end

    assert_redirected_to contact_path(contact)
    assert_equal "Enviar propuesta", contact.notes.last.body
  end

  test "returns 422 with note validation errors" do
    contact = Contact.create!(name: "Ana", email: "ana@example.com", status: "lead")

    post contact_notes_path(contact), params: { note: { body: "" } }

    assert_response :unprocessable_entity
    assert_includes response.body, "Revisa la nota"
  end

  test "exports contacts as downloadable CSV" do
    Contact.create!(name: "Ana", email: "ana@example.test", status: "active")

    get export_contacts_path

    assert_response :success
    assert_equal "text/csv", response.media_type
    assert_includes response.headers.fetch("Content-Disposition"), "contactdesk-contacts.csv"
    assert_includes response.body, "ana@example.test"
  end

  test "imports a valid CSV upload" do
    upload = csv_upload("name,email,company,status\nAna,ana@example.test,Acme,active\n")

    assert_difference("Contact.count", 1) do
      post import_contacts_path, params: { file: upload }
    end

    assert_redirected_to contacts_path
    assert_equal "active", Contact.find_by!(email: "ana@example.test").status
  ensure
    upload&.close
  end

  test "rejects invalid CSV without partial persistence" do
    upload = csv_upload("name,email,company,status\nAna,ana@example.test,,lead\nRoto,bad,,active\n")

    assert_no_difference("Contact.count") do
      post import_contacts_path, params: { file: upload }
    end

    assert_redirected_to contacts_path
    assert_includes flash[:alert], "Fila 3"
  ensure
    upload&.close
  end

  private

  def csv_upload(content)
    file = Tempfile.new(["contacts", ".csv"])
    file.write(content)
    file.close
    Rack::Test::UploadedFile.new(file.path, "text/csv", false, original_filename: "contacts.csv")
  end
end
