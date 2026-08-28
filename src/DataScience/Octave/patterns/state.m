function state(); current='closed'; if strcmp(current,'closed'); current='open'; else; current='closed'; end; assert(strcmp(current,'open')); end
