# splice

    Code
      p$constraints
    Output
      An object containing 4 linear constraints.
      
      #unnamed_constraint
      | x >= 5
      | Rows = 1
      
         x dir rhs
         1 >=  5  
      
      k
      | for (i in 1:3) { ... }
      | Rows = 3
      
               x dir rhs
        k[i=1] 1 <=  2  
        k[i=2] 2 <=  4  
        k[i=3] 3 <=  6  
      

# masking

    Code
      p1$constraints
    Output
      An object containing 3 linear constraints.
      
      #unnamed_constraint
      | for (i in 1:3) x * 4 <= i
      | Rows = 3
      
              x dir rhs
        [i=1] 4 <=  1  
        [i=2] 4 <=  2  
        [i=3] 4 <=  3  
      

---

    Code
      p2$constraints
    Output
      An object containing 3 linear constraints.
      
      #unnamed_constraint
      | for (i in 1:3) { ... }
      | Rows = 3
      
              x dir rhs
        [i=1] 1 <=  3  
        [i=2] 1 <=  6  
        [i=3] 1 <=  9  
      

---

    Code
      p3$aliases$s
    Output
      $L
           x[1] x[2] x[3] i
      [1,]    1    2    3 0
      with class 'robust_index' from package 'lpsugar'
      
      $A
           [,1]
      [1,]    0
      with class 'robust_index' from package 'lpsugar'
      

