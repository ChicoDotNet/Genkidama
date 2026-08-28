object ModelViewPresenterExample { def run:Boolean={var count=0;var text="";def present():Unit={count+=1;text=s"count=$count"};present();count==1&&text=="count=1"} }
