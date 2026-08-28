stock=fn->true end; charge=fn->:paid end; checkout=fn->if stock.(),do: charge.(),else: :sold_out end; unless checkout.()==:paid,do: raise "Facade"
