function adapter()
  read_fahrenheit = @() 86;
  read_celsius = @() round((read_fahrenheit() - 32) * 5 / 9);

  printf("legacy=%dF\n", read_fahrenheit());
  printf("adapted=%dC\n", read_celsius());
end
