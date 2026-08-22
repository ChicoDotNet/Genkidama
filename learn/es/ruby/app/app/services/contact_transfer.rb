require "csv"

# Imports and exports ContactDesk contacts through a bounded CSV contract.
#
# The service keeps parsing, validation and transaction behavior outside the HTTP
# controller so the same rules can be tested without a browser.
class ContactTransfer
  MAX_BYTES = 256 * 1024
  HEADERS = %w[name email company status].freeze

  class ImportError < StandardError; end

  # Serializes contacts using the canonical transfer columns.
  # @param scope [Enumerable<Contact>] contacts to export
  # @return [String] UTF-8 CSV including headers
  def self.export_csv(scope = Contact.order(:name))
    CSV.generate(headers: true) do |csv|
      csv << HEADERS
      scope.each { |contact| csv << HEADERS.map { |header| contact.public_send(header) } }
    end
  end

  # Imports contacts atomically from an IO-like CSV source.
  # Existing email addresses are updated; new addresses create contacts.
  # @param io [#read] CSV source
  # @return [Integer] number of processed rows
  # @raise [ImportError] when the file is too large, malformed or violates the contact contract
  def self.import_csv(io)
    payload = io.read(MAX_BYTES + 1)
    raise ImportError, "El archivo supera #{MAX_BYTES} bytes." if payload.bytesize > MAX_BYTES

    table = CSV.parse(payload, headers: true)
    missing = HEADERS - Array(table.headers)
    raise ImportError, "Faltan columnas: #{missing.join(', ')}." unless missing.empty?

    processed = 0
    Contact.transaction do
      table.each_with_index do |row, index|
        email = row["email"].to_s.strip.downcase
        contact = Contact.find_or_initialize_by(email: email)
        contact.assign_attributes(
          name: row["name"].to_s.strip,
          company: row["company"].to_s.strip.presence,
          status: row["status"].to_s.strip
        )

        next processed += 1 if contact.save

        raise ImportError, "Fila #{index + 2}: #{contact.errors.full_messages.join(', ')}."
      end
    end
    processed
  rescue CSV::MalformedCSVError => e
    raise ImportError, "CSV inválido: #{e.message}"
  end
end
