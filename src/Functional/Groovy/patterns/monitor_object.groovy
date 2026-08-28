def lock=new Object();def counter=0;synchronized(lock){counter++};assert counter==1
