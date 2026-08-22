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

  test "searches name email and company case-insensitively" do
    ana = Contact.create!(name: "Ana Torres", email: "ana@example.com", company: "Norte", status: "lead")
    Contact.create!(name: "Beatriz", email: "ventas@sur.example", company: "Sur", status: "active")

    assert_equal [ana], Contact.search("NORTE").to_a
    assert_equal [ana], Contact.search("ana@example").to_a
  end

  test "filters by status and keeps blank filter neutral" do
    lead = Contact.create!(name: "Ana", email: "ana@example.com", status: "lead")
    active = Contact.create!(name: "Beatriz", email: "bea@example.com", status: "active")

    assert_equal [active], Contact.with_status("active").to_a
    assert_equal [lead, active].sort_by(&:id), Contact.with_status(nil).order(:id).to_a
  end
end
