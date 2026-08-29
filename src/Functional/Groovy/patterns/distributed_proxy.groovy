def remote={id->[id:id,name:'Ada']};def proxy={id->remote(id).name};assert proxy(7)=='Ada'
