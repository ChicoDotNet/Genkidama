require "test_helper"

class HealthFlowTest < ActionDispatch::IntegrationTest
  test "returns operational health without exposing contact details" do
    Contact.create!(name: "Ana", email: "ana@example.test", status: "lead")

    get health_path

    assert_response :success
    payload = JSON.parse(response.body)
    assert_equal "ok", payload.fetch("status")
    assert_equal "ok", payload.fetch("database")
    assert_equal 1, payload.fetch("contacts")
    assert payload.fetch("request_id").present?
    refute_includes response.body, "ana@example.test"
  end
end
