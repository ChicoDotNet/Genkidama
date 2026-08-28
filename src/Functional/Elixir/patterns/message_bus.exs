bus=%{paid:[fn v->{:seen,v} end]}; unless Enum.map(bus.paid,& &1.(42))==[{:seen,42}],do: raise "MessageBus"
