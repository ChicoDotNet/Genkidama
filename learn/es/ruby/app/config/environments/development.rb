Rails.application.configure do
  config.eager_load = false
  config.consider_all_requests_local = true
  config.action_controller.perform_caching = false
end
