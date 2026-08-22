require "test_helper"

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
end
