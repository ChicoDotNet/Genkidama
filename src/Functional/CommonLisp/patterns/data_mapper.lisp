(flet((map-row(row)(list :name row)))(assert(string=(getf(map-row "Ada"):name)"Ada")))
