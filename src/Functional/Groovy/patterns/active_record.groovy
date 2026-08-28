def table=[:];def save={r->table[r.id]=[name:r.name]};save([id:1,name:'Ada']);assert table[1].name=='Ada'
