# printing

    Code
      print(plong$constraints, compact = TRUE)
    Output
      
       <unnamed> | n = 1 | x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x +  ...

---

    Code
      print(p$constraints, compact = TRUE)
    Output
      
       <unnamed> | n = 1 | x[1] == 0
       my_con | n = 6 | x < t(y)
       one_line_fs | n = 6 | for (i in 1:3) x[i, ] >= y[, i] - 10
       my_fs | n = 6 | for (i in 1:3) { ... }

---

    Code
      print(p_many_rows, compact = FALSE)
    Output
      <lpsugar Linear Problem>
      
      -- $variables --
      $y
      Real variable 'y[1:3]'
      
      -- $constraints --
       <unnamed> | n = 20 | for (i in 1:20) y[i%%3 + 1] <= i
      
             y[1] y[2] y[3] dir   
      [i=1]  0    1    0    <=  1 
      [i=2]  0    0    1    <=  2 
      [i=3]  1    0    0    <=  3 
      [i=4]  0    1    0    <=  4 
      [i=5]  0    0    1    <=  5 
      [i=6]  1    0    0    <=  6 
      [i=7]  0    1    0    <=  7 
      [i=8]  0    0    1    <=  8 
      [i=9]  1    0    0    <=  9 
      [i=10] 0    1    0    <=  10
      [i=11] 0    0    1    <=  11
      [i=12] 1    0    0    <=  12
      [i=13] 0    1    0    <=  13
      [i=14] 0    0    1    <=  14
      [i=15] 1    0    0    <=  15
      [i=16] 0    1    0    <=  16
      [i=17] 0    0    1    <=  17
      [i=18] 1    0    0    <=  18
      [i=19] 0    1    0    <=  19
      [i=20] 0    0    1    <=  20
      
      

---

    Code
      print(p_many_cols, compact = FALSE)
    Output
      <lpsugar Linear Problem>
      
      -- $variables --
      $z
      Real variable 'z[1:300]'
      
      -- $constraints --
    Message
      Problem has over 200 variables, printing with `compact = TRUE`.
    Output
      
       <unnamed> | n = 300 | z >= 0
       <unnamed> | n = 300 | z <= 10

# bind constraints

    Code
      q$constraints
    Output
      
       name_outer | n = 6 | for (i in 1:n) bind_cons(y[i] >= l + is_two[i] * (2 - l), y[i] <=  ...
      
                      y[1] y[2] y[3] is_two[1] is_two[2] is_two[3] dir  
      name_outer[i=1] 1    0    0    -2        0         0         >=  0
      name_outer[i=1] 1    0    0    3         0         0         <=  5
      name_outer[i=2] 0    1    0    0         -2        0         >=  0
      name_outer[i=2] 0    1    0    0         3         0         <=  5
      name_outer[i=3] 0    0    1    0         0         -2        >=  0
      name_outer[i=3] 0    0    1    0         0         3         <=  5
      

# conditional constraints

    Code
      p_if_for <- p
      for (i in 1:n) {
        p_if_for <- lp_con(p_if_for, cc = if (cond[i]) x[i] <= 0)
      }
      p_if_for$constraints
    Output
      
       cc | n = 3 | if (cond[i]) x[i] <= 0
      
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
      
       cc | n = 3 | for (i in seq_along(x)) if (cond[i]) { ... }
      
              x[1] x[2] x[3] x[4] x[5] dir  
      cc[i=2] 0    1    0    0    0    <=  0
      cc[i=3] 0    0    1    0    0    <=  0
      cc[i=5] 0    0    0    0    1    <=  0
      

# quadruple for

    Code
      rownames(p$constraints)
    Output
       [1] "[i=1][j=1][k=1][m=1]" "[i=1][j=1][k=1][m=2]" "[i=1][j=1][k=2][m=1]"
       [4] "[i=1][j=1][k=2][m=2]" "[i=1][j=2][k=1][m=1]" "[i=1][j=2][k=1][m=2]"
       [7] "[i=1][j=2][k=2][m=1]" "[i=1][j=2][k=2][m=2]" "[i=2][j=1][k=1][m=1]"
      [10] "[i=2][j=1][k=1][m=2]" "[i=2][j=1][k=2][m=1]" "[i=2][j=1][k=2][m=2]"
      [13] "[i=2][j=2][k=1][m=1]" "[i=2][j=2][k=1][m=2]" "[i=2][j=2][k=2][m=1]"
      [16] "[i=2][j=2][k=2][m=2]" "[i=3][j=1][k=1][m=1]" "[i=3][j=1][k=1][m=2]"
      [19] "[i=3][j=1][k=2][m=1]" "[i=3][j=1][k=2][m=2]" "[i=3][j=2][k=1][m=1]"
      [22] "[i=3][j=2][k=1][m=2]" "[i=3][j=2][k=2][m=1]" "[i=3][j=2][k=2][m=2]"

