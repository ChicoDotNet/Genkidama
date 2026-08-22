module Contactdesk
  class Diagnostics
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
