module Contactdesk
  # Produces a minimal operational snapshot for health checks and maintenance.
  # The snapshot intentionally contains aggregate counts only, never contact PII.
  class Diagnostics
    # Checks database reachability and returns a deterministic snapshot shape.
    # @param now [Time, ActiveSupport::TimeWithZone] clock value used for the timestamp
    # @return [Hash] status, database state, aggregate contact count and UTC timestamp
    # @note Database errors are converted to a degraded snapshot and logged by class only.
    # @note This method is read-only; it does not modify contacts or database configuration.
    def self.snapshot(now: Time.current)
      database_ok = ActiveRecord::Base.connection.select_value("SELECT 1").to_i == 1

      {
        status: database_ok ? "ok" : "degraded",
        database: database_ok ? "ok" : "error",
        contacts: Contact.count,
        generated_at: now.utc.iso8601
      }
    rescue ActiveRecord::ActiveRecordError => e
      Rails.logger.error("contactdesk.diagnostics database_error=#{e.class}")
      {
        status: "degraded",
        database: "error",
        contacts: nil,
        generated_at: now.utc.iso8601
      }
    end
  end
end
