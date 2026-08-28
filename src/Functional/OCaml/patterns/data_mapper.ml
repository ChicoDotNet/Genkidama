type user={name:string};;let mapper row={name=row};;let ()=assert((mapper"Ada").name="Ada")
