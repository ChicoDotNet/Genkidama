class Note < ApplicationRecord
  belongs_to :contact

  validates :body, presence: true, length: { maximum: 1000 }
end
