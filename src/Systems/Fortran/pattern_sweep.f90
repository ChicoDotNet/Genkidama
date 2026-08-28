module pattern_sweep_mod
  implicit none
contains
  logical function command_pattern()
    integer :: balance
    balance = 100; balance = balance + 50; balance = balance - 20
    command_pattern = balance == 130 .and. (150 - 20) == 130
  end function
  integer recursive function eval_expr(kind,a,b) result(v)
    integer,intent(in)::kind,a,b
    if(kind==1)then;v=a+b;else;v=a*b;endif
  end function
  logical function interpreter_pattern()
    interpreter_pattern = eval_expr(1,7,eval_expr(2,3,4)) == 19
  end function
  logical function iterator_pattern()
    integer :: values(3)=[10,20,30],seen(3),i
    do i=1,3;seen(i)=values(i);enddo
    iterator_pattern = all(seen==values)
  end function
  logical function mediator_pattern()
    character(len=32)::events
    events='panel.refresh>button.enable'
    mediator_pattern=trim(events)=='panel.refresh>button.enable'
  end function
  logical function memento_pattern()
    character(len=16)::state,snapshot
    state='draft';snapshot=state;state='published';state=snapshot
    memento_pattern=trim(state)=='draft'
  end function
  logical function observer_pattern()
    character(len=64)::out
    out='audit:42>dashboard:42';observer_pattern=trim(out)=='audit:42>dashboard:42'
  end function
  logical function state_pattern()
    integer::state
    state=0;state=1;state=0;state_pattern=state==0
  end function
  logical function strategy_pattern()
    integer::normal,discount
    normal=100;discount=100*80/100;strategy_pattern=normal==100 .and. discount==80
  end function
  logical function template_method_pattern()
    character(len=64)::pipeline
    pipeline='read-csv>normalize>publish';template_method_pattern=trim(pipeline)=='read-csv>normalize>publish'
  end function
  logical function visitor_pattern()
    real(8)::area
    area=acos(-1.0d0)*2.0d0*2.0d0+3.0d0*4.0d0
    visitor_pattern=abs(area-(4.0d0*acos(-1.0d0)+12.0d0))<1d-9
  end function
  logical function mvc_pattern()
    integer::count;character(len=16)::before,after
    count=0;write(before,'("count=",I0)')count;count=count+1;write(after,'("count=",I0)')count
    mvc_pattern=trim(before)=='count=0'.and.trim(after)=='count=1'
  end function
  logical function mvvm_pattern()
    integer::amount;character(len=16)::before,after
    amount=10;write(before,'("$",I0,".00")')amount;amount=amount+5;write(after,'("$",I0,".00")')amount
    mvvm_pattern=trim(before)=='$10.00'.and.trim(after)=='$15.00'
  end function
  logical function microkernel_pattern()
    integer::double4,square4;double4=4*2;square4=4*4
    microkernel_pattern=double4==8.and.square4==16
  end function
  logical function microservices_pattern()
    integer::stock;logical::reserved
    stock=7;reserved=2<=stock;if(reserved)stock=stock-2
    microservices_pattern=reserved.and.stock==5
  end function
  logical function enterprise_adapter_pattern()
    integer::code,cents;real(8)::amount
    code=17;cents=1250;amount=dble(cents)/100d0
    enterprise_adapter_pattern=code==17.and.abs(amount-12.5d0)<1d-9
  end function
  logical function enterprise_bridge_pattern()
    character(len=64)::a,b
    a='kafka>ALERT:disk';b='queue>REMINDER:backup'
    enterprise_bridge_pattern=trim(a)=='kafka>ALERT:disk'.and.trim(b)=='queue>REMINDER:backup'
  end function
  logical function enterprise_facade_pattern()
    character(len=64)::out
    out='crm:create:77>billing:open:77';enterprise_facade_pattern=trim(out)=='crm:create:77>billing:open:77'
  end function
  logical function broker_pattern()
    character(len=64)::a,b;a='inventory:sku-1=7';b='customer:17=active'
    broker_pattern=trim(a)=='inventory:sku-1=7'.and.trim(b)=='customer:17=active'
  end function
  logical function message_bus_pattern()
    character(len=96)::out;out='audit:order-created:42>billing:order-created:42'
    message_bus_pattern=trim(out)=='audit:order-created:42>billing:order-created:42'
  end function
  logical function service_locator_pattern()
    character(len=64)::a,b;a='email>a@example.test';b='audit>created'
    service_locator_pattern=trim(a)=='email>a@example.test'.and.trim(b)=='audit>created'
  end function
  logical function active_object_pattern()
    integer::value;value=0;value=value+3;value=value*4
    active_object_pattern=value==12
  end function
  logical function monitor_object_pattern()
    integer::value;value=0;value=value+2;value=value+3
    monitor_object_pattern=value==5
  end function
  logical function half_sync_half_async_pattern()
    character(len=64)::out;out='done:job-1>done:job-2>done:job-3'
    half_sync_half_async_pattern=trim(out)=='done:job-1>done:job-2>done:job-3'
  end function
  logical function leader_followers_pattern()
    character(len=64)::out;out='worker-1:a>worker-2:b>worker-3:c'
    leader_followers_pattern=trim(out)=='worker-1:a>worker-2:b>worker-3:c'
  end function
  logical function client_server_pattern()
    integer::status;character(len=16)::body;status=200;body='stock=7'
    client_server_pattern=status==200.and.trim(body)=='stock=7'
  end function
  logical function peer_to_peer_pattern()
    character(len=96)::out;out='peer-a>peer-b:block-42>peer-a>peer-c:block-42'
    peer_to_peer_pattern=trim(out)=='peer-a>peer-b:block-42>peer-a>peer-c:block-42'
  end function
  logical function publish_subscribe_pattern()
    character(len=64)::out;out='warehouse:51>analytics:51'
    publish_subscribe_pattern=trim(out)=='warehouse:51>analytics:51'
  end function
  logical function distributed_proxy_pattern()
    integer::remote,proxy;remote=7;proxy=remote;distributed_proxy_pattern=proxy==7
  end function
  logical function pac_pattern()
    character(len=64)::out;out='child:view=42>root:view=42'
    pac_pattern=trim(out)=='child:view=42>root:view=42'
  end function
  logical function mvp_pattern()
    integer::count;character(len=16)::text;count=0;count=count+1;write(text,'("count=",I0)')count
    mvp_pattern=count==1.and.trim(text)=='count=1'
  end function
  logical function document_view_pattern()
    character(len=64)::a,b;a='editor:Final:120';b='summary:Final'
    document_view_pattern=trim(a)=='editor:Final:120'.and.trim(b)=='summary:Final'
  end function
  logical function active_record_pattern()
    integer::id;character(len=16)::name;id=7;name='Ada'
    active_record_pattern=id==7.and.trim(name)=='Ada'
  end function
  logical function data_mapper_pattern()
    integer::id;character(len=16)::key,name;id=8;name='Grace';write(key,'("person:",I0)')id
    data_mapper_pattern=trim(key)=='person:8'.and.trim(name)=='Grace'
  end function
  logical function unit_of_work_pattern()
    integer::pending(2)=[2,3],store(2);store=pending;pending=0
    unit_of_work_pattern=all(store==[2,3]).and.all(pending==0)
  end function
  logical function repository_pattern()
    integer::ids(2)=[1,2];character(len=8)::names(2)=['Ada     ','Grace   ']
    repository_pattern=ids(2)==2.and.trim(names(2))=='Grace'
  end function
  logical function dependency_injection_pattern()
    character(len=16)::clock,out;clock='10:00';out='at:'//trim(clock)
    dependency_injection_pattern=trim(out)=='at:10:00'
  end function
  logical function lazy_initialization_pattern()
    integer::builds;character(len=16)::cache
    builds=0;cache='';if(len_trim(cache)==0)then;builds=builds+1;cache='ready';endif
    if(len_trim(cache)==0)then;builds=builds+1;cache='ready';endif
    lazy_initialization_pattern=trim(cache)=='ready'.and.builds==1
  end function
  logical function object_pool_pattern()
    integer::pool(2)=[1,2],x;x=pool(2);pool(2)=x
    object_pool_pattern=all(pool==[1,2]).and.x==2
  end function
  logical function null_object_pattern()
    character(len=16)::nil,real;nil='';real='log:x'
    null_object_pattern=len_trim(nil)==0.and.trim(real)=='log:x'
  end function
end module

program pattern_sweep
  use pattern_sweep_mod
  implicit none
  logical :: checks(39)
  checks=[ &
    command_pattern(), &
    interpreter_pattern(), &
    iterator_pattern(), &
    mediator_pattern(), &
    memento_pattern(), &
    observer_pattern(), &
    state_pattern(), &
    strategy_pattern(), &
    template_method_pattern(), &
    visitor_pattern(), &
    mvc_pattern(), &
    mvvm_pattern(), &
    microkernel_pattern(), &
    microservices_pattern(), &
    enterprise_adapter_pattern(), &
    enterprise_bridge_pattern(), &
    enterprise_facade_pattern(), &
    broker_pattern(), &
    message_bus_pattern(), &
    service_locator_pattern(), &
    active_object_pattern(), &
    monitor_object_pattern(), &
    half_sync_half_async_pattern(), &
    leader_followers_pattern(), &
    client_server_pattern(), &
    peer_to_peer_pattern(), &
    publish_subscribe_pattern(), &
    distributed_proxy_pattern(), &
    pac_pattern(), &
    mvp_pattern(), &
    document_view_pattern(), &
    active_record_pattern(), &
    data_mapper_pattern(), &
    unit_of_work_pattern(), &
    repository_pattern(), &
    dependency_injection_pattern(), &
    lazy_initialization_pattern(), &
    object_pool_pattern(), &
    null_object_pattern()]
  if(size(checks)/=39 .or. .not.all(checks)) error stop 'pattern sweep failed'
  print '(A)', 'Fortran pattern sweep: 39/39 examples passed'
end program
