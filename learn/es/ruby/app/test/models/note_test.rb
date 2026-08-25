require "test_helper"

class NoteTest < ActiveSupport::TestCase
  test "requires a contact and body" do
    note = Note.new(body: "")

    refute note.valid?
    assert_includes note.errors[:contact], "must exist"
    assert_includes note.errors[:body], "can't be blank"
  end

  test "accepts a note attached to a contact" do
    contact = Contact.create!(name: "Ana", email: "ana@example.com", status: "lead")

    assert contact.notes.build(body: "Llamar el viernes").valid?
  end
end
