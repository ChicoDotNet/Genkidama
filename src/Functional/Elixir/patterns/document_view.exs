doc=%{title:"One"}; a=fn d->d.title end;b=fn d->String.upcase(d.title) end;unless {a.(doc),b.(doc)}=={"One","ONE"},do: raise "DocumentView"
