function half_sync_half_async(); incoming={'a','b'}; completed=cellfun(@upper,incoming,'UniformOutput',false); assert(isequal(completed,{'A','B'})); end
