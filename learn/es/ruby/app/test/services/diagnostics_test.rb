require "test_helper"

class DiagnosticsTest < ActiveSupport::TestCase
  test "returns a healthy snapshot without contact PII" do
    Contact.create!(name: "Ana", email: "ana@example.test", status: "lead")

    snapshot = Contactdesk::Diagnostics.snapshot(now: Time.utc(2026, 8, 22, 12, 0, 0))

    assert_equal "ok", snapshot[:status]
    assert_equal "ok", snapshot[:database]
    assert_equal 1, snapshot[:contacts]
    assert_equal "2026-08-22T12:00:00Z", snapshot[:generated_at]
    refute_includes snapshot.to_s, "ana@example.test"
  end
end
