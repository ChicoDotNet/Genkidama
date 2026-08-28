object MvcExample { def run:Boolean={var c=0;def view=s"count=$c";val before=view;c+=1;before=="count=0"&&view=="count=1"} }
