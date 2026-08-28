let ()=let model="Ada"and view=ref""in let presenter()=view:=String.uppercase_ascii model in presenter();assert(!view="ADA")
