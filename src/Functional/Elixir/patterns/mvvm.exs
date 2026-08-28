vm=fn m->"#{m.first} #{m.last}" end; unless vm.(%{first:"Ada",last:"Lovelace"})=="Ada Lovelace",do: raise "MVVM"
