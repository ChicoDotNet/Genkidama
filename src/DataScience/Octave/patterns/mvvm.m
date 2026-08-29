function mvvm(); model.first='Ada';model.last='Lovelace';vm=@(m)[m.first ' ' m.last];assert(strcmp(vm(model),'Ada Lovelace'));end
