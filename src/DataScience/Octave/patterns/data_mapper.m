function data_mapper(); row.name='Ada'; mapper=@(r)struct('name',r.name); user=mapper(row); assert(strcmp(user.name,'Ada')); end
