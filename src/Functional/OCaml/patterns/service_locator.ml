let services=[("clock",fun()->"12:00")];;let ()=assert((List.assoc"clock"services)()="12:00")
