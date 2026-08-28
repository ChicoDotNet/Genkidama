function pacPattern(){const view=(name:string,value:number)=>`${name}:view=${value}`;return view('child',42)==='child:view=42'&&view('root',42)==='root:view=42'}
