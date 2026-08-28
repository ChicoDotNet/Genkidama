function mvcPattern(){let count=0;const view=()=>`count=${count}`;const before=view();count++;return before==='count=0'&&view()==='count=1'}
