require "test_helper"
require "stringio"

class ContactTransferTest < ActiveSupport::TestCase
  test "exporta columnas canonicas y datos" do
    Contact.create!(name: "Ana", email: "ana@example.test", company: "Acme", status: "active")

    csv = ContactTransfer.export_csv(Contact.where(email: "ana@example.test"))

    assert_includes csv, "name,email,company,status"
    assert_includes csv, "Ana,ana@example.test,Acme,active"
  end

  test "importa filas validas y actualiza por email" do
    Contact.create!(name: "Anterior", email: "ana@example.test", status: "lead")
    source = StringIO.new("name,email,company,status\nAna,ANA@example.test,Acme,active\n")

    assert_equal 1, ContactTransfer.import_csv(source)

    contact = Contact.find_by!(email: "ana@example.test")
    assert_equal "Ana", contact.name
    assert_equal "active", contact.status
    assert_equal 1, Contact.where(email: "ana@example.test").count
  end

  test "revierte toda la importacion si una fila es invalida" do
    source = StringIO.new("name,email,company,status\nValido,ok@example.test,,lead\nRoto,no-es-email,,active\n")

    assert_raises(ContactTransfer::ImportError) { ContactTransfer.import_csv(source) }
    assert_not Contact.exists?(email: "ok@example.test")
  end

  test "rechaza columnas faltantes" do
    source = StringIO.new("name,email\nAna,ana@example.test\n")

    error = assert_raises(ContactTransfer::ImportError) { ContactTransfer.import_csv(source) }
    assert_includes error.message, "Faltan columnas"
  end

  test "rechaza archivos que superan el limite" do
    source = StringIO.new("x" * (ContactTransfer::MAX_BYTES + 1))

    error = assert_raises(ContactTransfer::ImportError) { ContactTransfer.import_csv(source) }
    assert_includes error.message, "supera"
  end
end
