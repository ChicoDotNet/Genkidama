def model=[count:0];def controller={model.count++};def view={"count=${model.count}"};controller();assert view()=='count=1'
