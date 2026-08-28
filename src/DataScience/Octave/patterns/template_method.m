function template_method(); render=@(body)['<' body() '>']; assert(strcmp(render(@()'sales'),'<sales>')); end
