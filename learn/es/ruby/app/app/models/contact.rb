class Contact < ApplicationRecord
  has_many :notes, dependent: :destroy

  validates :name, presence: true
  validates :email, presence: true, uniqueness: true, format: { with: URI::MailTo::EMAIL_REGEXP }
  validates :status, inclusion: { in: %w[lead active archived] }

  # Filters contacts by a case-insensitive term across the fields people search most often.
  # @param term [String, nil] text supplied by the user
  # @return [ActiveRecord::Relation<Contact>] matching contacts, or all contacts when blank
  def self.search(term)
    normalized = term.to_s.strip.downcase
    return all if normalized.empty?

    pattern = "%#{sanitize_sql_like(normalized)}%"
    where(
      "LOWER(name) LIKE :pattern OR LOWER(email) LIKE :pattern OR LOWER(COALESCE(company, '')) LIKE :pattern",
      pattern: pattern
    )
  end

  # Filters contacts by lifecycle status without forcing callers to branch.
  # @param status [String, nil] lead, active or archived
  # @return [ActiveRecord::Relation<Contact>] filtered or unfiltered relation
  def self.with_status(status)
    return all if status.blank?

    where(status: status)
  end
end
