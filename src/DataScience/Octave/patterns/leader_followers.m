function leader_followers(); workers={'leader','follower'};events={'one','two'};assert(strcmp([workers{1} ':' events{1}],'leader:one')&&strcmp([workers{2} ':' events{2}],'follower:two'));end
