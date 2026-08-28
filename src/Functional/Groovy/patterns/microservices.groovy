def inventory={sku->[sku:sku,available:true]};def order={sku->inventory(sku).available};assert order('A-1')
