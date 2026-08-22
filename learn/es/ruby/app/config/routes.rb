Rails.application.routes.draw do
  root "contacts#index"
  get "/healthz", to: "health#show", as: :health

  resources :contacts, only: %i[index show new create edit update] do
    collection do
      get :export
      post :import
    end
    resources :notes, only: :create
  end
end
