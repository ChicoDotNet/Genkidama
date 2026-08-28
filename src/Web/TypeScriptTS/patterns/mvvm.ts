function mvvmPattern(){let amount=10;const text=()=>`$${amount}.00`;const before=text();amount+=5;return before==='$10.00'&&text()==='$15.00'}
