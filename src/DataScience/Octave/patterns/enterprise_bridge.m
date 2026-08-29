function enterprise_bridge(); sender=@(x)['sms:' x]; notify=@(x)sender(x); assert(strcmp(notify('ok'),'sms:ok')); end
