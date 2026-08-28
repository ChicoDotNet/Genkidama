function templateMethodPattern(){const pipeline=(read:string,transform:()=>string)=>`${read}>${transform()}>publish`;return pipeline('read-csv',()=> 'normalize')==='read-csv>normalize>publish'}
