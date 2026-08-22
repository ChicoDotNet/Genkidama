function adapter
    readFahrenheit = @() 86;
    readCelsius = @() round((readFahrenheit() - 32) * 5 / 9);

    fprintf('legacy=%dF\n', readFahrenheit());
    fprintf('adapted=%dC\n', readCelsius());
end
