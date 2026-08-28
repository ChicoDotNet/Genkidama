let ()=let events=ref[] in let med s m=events:=(s^":"^m)::!events in med"checkout""paid";assert(!events=["checkout:paid"])
