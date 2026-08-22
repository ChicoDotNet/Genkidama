Rails.application.routes.draw do
  root "contacts#index"
  resources :contacts, only: %i[index new create]
end
