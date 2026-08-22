class CreateContacts < ActiveRecord::Migration[8.1]
  def change
    create_table :contacts do |t|
      t.string :name, null: false
      t.string :email, null: false
      t.string :company
      t.string :status, null: false, default: "lead"
      t.timestamps
    end

    add_index :contacts, :email, unique: true
  end
end
