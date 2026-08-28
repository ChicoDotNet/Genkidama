(flet((null-log(x)(declare(ignore x))nil)(service(logger)(funcall logger "run")'ok))(assert(eq(service #'null-log)'ok)))
