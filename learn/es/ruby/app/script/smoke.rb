# frozen_string_literal: true

abort "Contact model unavailable" unless defined?(Contact)

contact = Contact.new(
  name: "Smoke Contact",
  email: "smoke@example.test",
  status: "active"
)

abort "Contact validations unavailable" unless contact.valid?
