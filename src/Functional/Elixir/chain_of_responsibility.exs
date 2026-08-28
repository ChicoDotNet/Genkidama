defmodule HandlerChain do
  def route(amount, handlers) do
    result =
      Enum.reduce_while(handlers, [], fn handler, visited ->
        visited_now = visited ++ [handler.name]

        if handler.accepts.(amount) do
          {:halt, {visited_now, handler.name}}
        else
          {:cont, visited_now}
        end
      end)

    case result do
      {visited, handled} -> {visited, handled}
      _ -> raise "No handler accepted the request"
    end
  end
end

handlers = [
  %{name: "faq", accepts: fn amount -> amount <= 50 end},
  %{name: "billing", accepts: fn amount -> amount <= 500 end},
  %{name: "escalation", accepts: fn _ -> true end}
]

amount = 250
{visited, handled} = HandlerChain.route(amount, handlers)
IO.puts("visited=#{Enum.join(visited, ">");handled=#{handled};result=refund(#{amount})")
