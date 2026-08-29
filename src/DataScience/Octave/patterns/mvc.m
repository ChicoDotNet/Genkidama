function mvc(); model.count=0; model.count=model.count+1; view=@(m)sprintf('count=%d',m.count); assert(strcmp(view(model),'count=1')); end
