function interpreter(); env.x=4; ast={'add',{'var','x'},{'lit',3}}; assert(strcmp(ast{1},'add') && env.(ast{2}{2})+ast{3}{2}==7); end
