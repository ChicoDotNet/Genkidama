Rails.application.configure do
  config.eager_load = true
  config.consider_all_requests_local = false
  config.action_controller.perform_caching = true
  config.force_ssl = true
end
