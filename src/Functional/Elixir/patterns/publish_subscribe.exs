topics=%{news:[fn v->{:received,v} end]}; unless Enum.map(topics.news,& &1.("v1"))==[{:received,"v1"}],do: raise "PubSub"
