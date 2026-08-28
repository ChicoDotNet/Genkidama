def a=[value:1];def control={d->a.value+=d};def presentation={a.value.toString()};control(2);assert presentation()=='3'
