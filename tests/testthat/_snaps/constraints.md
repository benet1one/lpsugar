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

# conditional constraints

    Code
      p_if_for <- p
      for (i in 1:n) {
        p_if_for <- lp_con(p_if_for, cc = if (cond[i]) x[i] <= 0)
      }
      p_if_for$constraints
    Output
      
       cc | if (cond[i]) x[i] <= 0
      
         x[1] x[2] x[3] x[4] x[5] dir  
      cc 0    1    0    0    0    <=  0
      cc 0    0    1    0    0    <=  0
      cc 0    0    0    0    1    <=  0
      

---

    Code
      p_for_if <- lp_con(p, cc = for (i in seq_along(x)) if (cond[i]) {
        x[i] <= 0
      })
      p_for_if$constraints
    Output
      
       cc | for (i in seq_along(x)) if (cond[i]) { ... }
      
              x[1] x[2] x[3] x[4] x[5] dir  
      cc[i=2] 0    1    0    0    0    <=  0
      cc[i=3] 0    0    1    0    0    <=  0
      cc[i=5] 0    0    0    0    1    <=  0
      

