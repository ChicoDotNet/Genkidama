class CreateNotes < ActiveRecord::Migration[8.1]
  def change
    create_table :notes do |t|
      t.references :contact, null: false, foreign_key: true
      t.text :body, null: false
      t.timestamps
    end
  end
end
