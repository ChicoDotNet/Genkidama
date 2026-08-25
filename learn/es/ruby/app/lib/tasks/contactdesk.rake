namespace :contactdesk do
  desc "Print a read-only operational snapshot without contact PII"
  task diagnostics: :environment do
    snapshot = Contactdesk::Diagnostics.snapshot
    puts "status=#{snapshot[:status]} database=#{snapshot[:database]} contacts=#{snapshot[:contacts]} generated_at=#{snapshot[:generated_at]}"
    abort "ContactDesk diagnostics degraded" unless snapshot[:status] == "ok"
  end
end
