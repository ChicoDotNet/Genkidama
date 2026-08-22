require "test_helper"

class ContactTest < ActiveSupport::TestCase
  test "accepts a valid lead" do
    contact = Contact.new(name: "Ana Torres", email: "ana@example.com", company: "Norte", status: "lead")
    assert contact.valid?
  end

  test "requires name" do
    contact = Contact.new(email: "ana@example.com", status: "lead")
    refute contact.valid?
    assert_includes contact.errors[:name], "can't be blank"
  end

  test "rejects invalid email" do
    contact = Contact.new(name: "Ana", email: "no-es-correo", status: "lead")
    refute contact.valid?
  end

  test "rejects unknown status" do
    contact = Contact.new(name: "Ana", email: "ana@example.com", status: "unknown")
    refute contact.valid?
  end

  test "rejects duplicate email" do
    Contact.create!(name: "Ana", email: "ana@example.com", status: "lead")
    duplicate = Contact.new(name: "Otra Ana", email: "ana@example.com", status: "lead")
    refute duplicate.valid?
  end
end
