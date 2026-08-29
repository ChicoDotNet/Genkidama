plugins = %{upper: &String.upcase/1}

unless plugins.upper.("plugin") == "PLUGIN" do
  raise "Microkernel"
end
