# printing

    Code
      print(p$constraints, full = FALSE)
    Output
      An object containing 19 linear constraints.
      
      #unnamed_constraint
      | x[1] == 0
      | Rows = 1
      
      my_con
      | x < t(y)
      | Rows = 6
      
      one_line_fs
      | for (i in 1:3) x[i, ] >= y[, i] - 10
      | Rows = 6
      
      my_fs
      | for (i in 1:3) { ... }
      | Rows = 6
      

---

    Code
      print(p$constraints)
    Output
      An object containing 19 linear constraints.
      
      #unnamed_constraint
      | x[1] == 0
      | Rows = 1
      
         x[1,1] x[2,1] x[3,1] x[1,2] x[2,2] x[3,2] y[1,1] y[2,1] y[1,2] y[2,2] y[1,3]
         1      0      0      0      0      0      0      0      0      0      0     
         y[2,3] dir rhs
         0      ==  0  
      
      my_con
      | x < t(y)
      | Rows = 6
      
               x[1,1] x[2,1] x[3,1] x[1,2] x[2,2] x[3,2] y[1,1] y[2,1] y[1,2] y[2,2]
        my_con 1      0      0      0      0      0      -1     0      0      0     
        my_con 0      1      0      0      0      0      0      0      -1     0     
        my_con 0      0      1      0      0      0      0      0      0      0     
        my_con 0      0      0      1      0      0      0      -1     0      0     
        my_con 0      0      0      0      1      0      0      0      0      -1    
        my_con 0      0      0      0      0      1      0      0      0      0     
               y[1,3] y[2,3] dir rhs
        my_con 0      0      <=  0  
        my_con 0      0      <=  0  
        my_con -1     0      <=  0  
        my_con 0      0      <=  0  
        my_con 0      0      <=  0  
        my_con 0      -1     <=  0  
      
      one_line_fs
      | for (i in 1:3) x[i, ] >= y[, i] - 10
      | Rows = 6
      
                         x[1,1] x[2,1] x[3,1] x[1,2] x[2,2] x[3,2] y[1,1] y[2,1]
        one_line_fs[i=1] 1      0      0      0      0      0      -1     0     
        one_line_fs[i=1] 0      0      0      1      0      0      0      -1    
        one_line_fs[i=2] 0      1      0      0      0      0      0      0     
        one_line_fs[i=2] 0      0      0      0      1      0      0      0     
        one_line_fs[i=3] 0      0      1      0      0      0      0      0     
        one_line_fs[i=3] 0      0      0      0      0      1      0      0     
                         y[1,2] y[2,2] y[1,3] y[2,3] dir rhs
        one_line_fs[i=1] 0      0      0      0      >=  -10
        one_line_fs[i=1] 0      0      0      0      >=  -10
        one_line_fs[i=2] -1     0      0      0      >=  -10
        one_line_fs[i=2] 0      -1     0      0      >=  -10
        one_line_fs[i=3] 0      0      -1     0      >=  -10
        one_line_fs[i=3] 0      0      0      -1     >=  -10
      
      my_fs
      | for (i in 1:3) { ... }
      | Rows = 6
      
                   x[1,1] x[2,1] x[3,1] x[1,2] x[2,2] x[3,2] y[1,1] y[2,1] y[1,2]
        my_fs[i=1] 1      0      0      0      0      0      -1     0      0     
        my_fs[i=1] 0      0      0      1      0      0      0      -1     0     
        my_fs[i=2] 0      1      0      0      0      0      0      0      -1    
        my_fs[i=2] 0      0      0      0      1      0      0      0      0     
        my_fs[i=3] 0      0      1      0      0      0      0      0      0     
        my_fs[i=3] 0      0      0      0      0      1      0      0      0     
                   y[2,2] y[1,3] y[2,3] dir rhs
        my_fs[i=1] 0      0      0      <=  0  
        my_fs[i=1] 0      0      0      <=  0  
        my_fs[i=2] 0      0      0      <=  0  
        my_fs[i=2] -1     0      0      <=  0  
        my_fs[i=3] 0      -1     0      <=  0  
        my_fs[i=3] 0      0      -1     <=  0  
      

---

    Code
      print(plong$constraints, full = FALSE)
    Output
      An object containing 1 linear constraint.
      
      #unnamed_constraint
      | x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x +  ...
      | Rows = 1
      

---

    Code
      print(plong$constraints)
    Output
      An object containing 1 linear constraint.
      
      #unnamed_constraint
      | x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x +  ...
      | Rows = 1
      
         x  dir rhs
         28 >=  1  
      

---

    Code
      print(p_many_rows, full = TRUE)
    Output
      ---- <lp_problem> ----
      
      -- $variables
      $y
      Real variable 'y[1:3]'
      
      -- $constraints
      An object containing 20 linear constraints.
      
      #unnamed_constraint
      | for (i in 1:20) y[i%%3 + 1] <= i
      | Rows = 20
      
               y[1] y[2] y[3] dir rhs
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
      print(p_many_cols, full = TRUE)
    Output
      ---- <lp_problem> ----
      
      -- $variables
      $z
      Real variable 'z[1:300]'
      
      -- $constraints
      An object containing 600 linear constraints.
      
      #unnamed_constraint
      | z >= 0
      | Rows = 300
      
      #unnamed_constraint
      | z <= 10
      | Rows = 300
      

# bind constraints

    Code
      q$constraints
    Output
      An object containing 6 linear constraints.
      
      name_outer
      | for (i in 1:n) bind_cons(y[i] >= l + is_two[i] * (2 - l), y[i] <=  ...
      | Rows = 6
      
                        y[1] y[2] y[3] is_two[1] is_two[2] is_two[3] dir rhs
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
      An object containing 3 linear constraints.
      
      cc
      | if (cond[i]) x[i] <= 0
      | Rows = 3
      
           x[1] x[2] x[3] x[4] x[5] dir rhs
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
      An object containing 3 linear constraints.
      
      cc
      | for (i in seq_along(x)) if (cond[i]) { ... }
      | Rows = 3
      
                x[1] x[2] x[3] x[4] x[5] dir rhs
        cc[i=2] 0    1    0    0    0    <=  0  
        cc[i=3] 0    0    1    0    0    <=  0  
        cc[i=5] 0    0    0    0    1    <=  0  
      

# quadruple for

    Code
      rownames(p$constraints)
    Output
       [1] "[i=1, j=1, k=1, m=1]" "[i=1, j=1, k=1, m=2]" "[i=1, j=1, k=2, m=1]"
       [4] "[i=1, j=1, k=2, m=2]" "[i=1, j=2, k=1, m=1]" "[i=1, j=2, k=1, m=2]"
       [7] "[i=1, j=2, k=2, m=1]" "[i=1, j=2, k=2, m=2]" "[i=2, j=1, k=1, m=1]"
      [10] "[i=2, j=1, k=1, m=2]" "[i=2, j=1, k=2, m=1]" "[i=2, j=1, k=2, m=2]"
      [13] "[i=2, j=2, k=1, m=1]" "[i=2, j=2, k=1, m=2]" "[i=2, j=2, k=2, m=1]"
      [16] "[i=2, j=2, k=2, m=2]" "[i=3, j=1, k=1, m=1]" "[i=3, j=1, k=1, m=2]"
      [19] "[i=3, j=1, k=2, m=1]" "[i=3, j=1, k=2, m=2]" "[i=3, j=2, k=1, m=1]"
      [22] "[i=3, j=2, k=1, m=2]" "[i=3, j=2, k=2, m=1]" "[i=3, j=2, k=2, m=2]"

