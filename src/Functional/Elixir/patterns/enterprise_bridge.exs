sender=fn x->"sms:#{x}" end; notify=fn x->sender.(x) end; unless notify.("ok")=="sms:ok",do: raise "Bridge"
