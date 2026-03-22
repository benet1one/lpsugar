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

# unique constraint names

    Code
      s$pointer
    Output
      Model name: 
                x[1]  x[2]  x[3]       
      Minimize     0     0     0       
      first[1]     1     0     0  >=  0
      first[2]     0     1     0  >=  0
      first[3]     0     0     1  >=  0
      R4           1     0     0   =  0
      R5           0     1     0   =  1
      fourth       0     0     1  <=  6
      fifth[1]     1     0     0  <=  8
      fifth[2]     0     1     0  <=  8
      fifth[3]     0     0     1  <=  8
      R10          1     0     0   =  5
      R11          0     1     0   =  5
      R12          0     0     1   =  5
      Kind       Std   Std   Std       
      Type      Real  Real  Real       
      Upper      Inf   Inf   Inf       
      Lower     -Inf  -Inf  -Inf       

