# splice

    Code
      p$constraints
    Output
      
       <unnamed> | n = 1 | x >= 5
      
       x dir  
       1 >=  5
      
      
       k | n = 3 | for (i in 1:3) { ... }
      
             x dir  
      k[i=1] 1 <=  2
      k[i=2] 2 <=  4
      k[i=3] 3 <=  6
      

# masking

    Code
      p1$constraints
    Output
      
       <unnamed> | n = 3 | for (i in 1:3) x * 4 <= i
      
            x dir  
      [i=1] 4 <=  1
      [i=2] 4 <=  2
      [i=3] 4 <=  3
      

---

    Code
      p2$constraints
    Output
      
       <unnamed> | n = 3 | for (i in 1:3) { ... }
      
            x dir  
      [i=1] 1 <=  3
      [i=2] 1 <=  6
      [i=3] 1 <=  9
      

---

    Code
      p3$aliases$s
    Output
      $coef
           x[1] x[2] x[3] i
      [1,]    1    2    3 0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    0
      with class 'robust_index' from package 'lpsugar'
      

