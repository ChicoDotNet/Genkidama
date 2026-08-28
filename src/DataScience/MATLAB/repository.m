function result = repository
%REPOSITORY Expose collection-like domain access over persistence details.
store = struct();
repo = makeRepository(store);
repo = repoAdd(repo, struct("id", 9, "name", "Linus"));
entity = repoGet(repo, 9);
result = entity;
end

function repo = makeRepository(store)
repo = struct("store", store);
end

function repo = repoAdd(repo, entity)
key = char("id" + string(entity.id));
repo.store.(key) = entity;
end

function entity = repoGet(repo, id)
key = char("id" + string(id));
entity = repo.store.(key);
end
