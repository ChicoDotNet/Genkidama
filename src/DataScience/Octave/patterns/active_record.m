function active_record(); record.id=1;record.name='Ada';table=containers.Map('KeyType','double','ValueType','any');table(record.id)=record;assert(strcmp(table(1).name,'Ada'));end
