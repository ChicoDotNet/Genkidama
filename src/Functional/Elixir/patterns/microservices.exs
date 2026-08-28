inventory=fn sku->%{sku:sku,available:true} end; order=fn sku->inventory.(sku).available end; unless order.("A-1"),do: raise "Microservices"
