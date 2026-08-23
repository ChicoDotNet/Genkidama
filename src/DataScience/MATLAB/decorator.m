function decorator()
    base = @() "alert";
    audit = @(inner) @() "audit(" + inner() + ")";
    encrypt = @(inner) @() "enc(" + inner() + ")";

    audited = audit(base);
    encrypted = encrypt(base);
    stacked = audit(encrypt(base));

    fprintf('base=%s\n', base());
    fprintf('audit=%s\n', audited());
    fprintf('encrypted=%s\n', encrypted());
    fprintf('stacked=%s\n', stacked());
end
