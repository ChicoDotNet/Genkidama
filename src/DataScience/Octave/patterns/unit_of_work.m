function unit_of_work(); pending={struct('id',1)};database=pending;pending={};assert(database{1}.id==1&&isempty(pending));end
