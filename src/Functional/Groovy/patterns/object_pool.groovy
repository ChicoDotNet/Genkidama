def item=[id:1];def pool=[item];def borrowed=pool.remove(0);pool<<borrowed;assert pool[0].is(item)
