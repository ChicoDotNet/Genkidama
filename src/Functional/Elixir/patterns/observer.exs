sub=fn e->{:seen,e} end; unless Enum.map([sub],& &1.(:changed))==[seen: :changed],do: raise "Observer"
