function presentation_abstraction_control(); abstraction.value=1;control=@(a,d)struct('value',a.value+d);abstraction=control(abstraction,2);assert(abstraction.value==3);end
