function publish_subscribe(); subscriber=@(v)['received:' v]; topics.news={subscriber}; assert(strcmp(topics.news{1}('v1'),'received:v1')); end
