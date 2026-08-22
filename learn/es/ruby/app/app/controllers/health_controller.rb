class HealthController < ApplicationController
  def show
    snapshot = Contactdesk::Diagnostics.snapshot
    status = snapshot[:status] == "ok" ? :ok : :service_unavailable

    render json: snapshot.merge(request_id: request.request_id), status: status
  end
end
