def calls=0,value=null;def get={if(value==null){calls++;value=new Object()};value};def a=get(),b=get();assert a.is(b)&&calls==1
