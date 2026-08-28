function object_pool(); pool={struct('id',1)}; item=pool{1}; pool={item}; assert(pool{1}.id==1); end
