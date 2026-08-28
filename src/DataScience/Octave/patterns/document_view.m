function document_view(); document.title='One';a=@(d)d.title;b=@(d)upper(d.title);assert(strcmp(a(document),'One')&&strcmp(b(document),'ONE'));end
