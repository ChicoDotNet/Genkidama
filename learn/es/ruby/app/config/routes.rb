Rails.application.routes.draw do
  root "contacts#index"

  resources :contacts, only: %i[index show new create edit update] do
    resources :notes, only: :create
  end
end
