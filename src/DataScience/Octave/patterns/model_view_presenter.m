function model_view_presenter(); model.name='Ada'; presenter=@(m)upper(m.name); view.text=presenter(model); assert(strcmp(view.text,'ADA')); end
