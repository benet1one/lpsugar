# printing

    Code
      print(plong$constraints, compact = TRUE)
    Output
      
       <unnamed> | x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x +  ... 

---

    Code
      print(p$constraints, compact = TRUE)
    Output
      
       <unnamed> | x[1] == 0
       my_con | x < t(y)
       one_line_fs | for (i in 1:3) x[i, ] >= y[, i] - 10
       my_fs | for (i in 1:3) { ... }

