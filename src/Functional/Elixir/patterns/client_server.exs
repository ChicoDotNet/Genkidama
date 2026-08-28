server = fn request -> %{echo: request} end
client = fn value -> server.(value).echo end

unless client.("ping") == "ping" do
  raise "ClientServer"
end
