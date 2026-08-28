def peers=[a:[],b:[]];def send={s,t,m->peers[t]<<[s,m]};send('a','b','hello');assert peers.b==[['a','hello']]
