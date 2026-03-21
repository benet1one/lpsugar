# diag

    Code
      lp_eval(p, diag(1, nrow = 3))
    Output
           [,1] [,2] [,3]
      [1,]    1    0    0
      [2,]    0    1    0
      [3,]    0    0    1

---

    Code
      lp_eval(p, diag(x))
    Output
      $coef
           x[1,1] x[2,1] x[1,2] x[2,2] diag
      [1,]      1      0      0      0    0
      [2,]      0      0      0      1    0
      attr(,"class")
      [1] "robust_index" "matrix"       "array"       
      
      $add
           [,1]
      [1,]    0
      [2,]    0
      attr(,"class")
      [1] "robust_index" "matrix"       "array"       
      

# ifelse2

    Code
      p$constraints
    Output
      
       <unnamed> | x < ifelse(1:n <= 2, cap_special, cap)
      
       x[1] x[2] x[3] x[4] x[5] x[6] cap[1] cap[2] cap[3] cap[4] cap[5] cap[6] cap_special dir  
       1    0    0    0    0    0    0      0      0      0      0      0      -1          <=  0
       0    1    0    0    0    0    0      0      0      0      0      0      -1          <=  0
       0    0    1    0    0    0    0      0      -1     0      0      0      0           <=  0
       0    0    0    1    0    0    0      0      0      -1     0      0      0           <=  0
       0    0    0    0    1    0    0      0      0      0      -1     0      0           <=  0
       0    0    0    0    0    1    0      0      0      0      0      -1     0           <=  0
      
      
       <unnamed> | x > ifelse(1:n <= 3, 1:n, cap_special/10)
      
       x[1] x[2] x[3] x[4] x[5] x[6] cap[1] cap[2] cap[3] cap[4] cap[5] cap[6] cap_special dir  
       1    0    0    0    0    0    0      0      0      0      0      0      0           >=  1
       0    1    0    0    0    0    0      0      0      0      0      0      0           >=  2
       0    0    1    0    0    0    0      0      0      0      0      0      0           >=  3
       0    0    0    1    0    0    0      0      0      0      0      0      -0.1        >=  0
       0    0    0    0    1    0    0      0      0      0      0      0      -0.1        >=  0
       0    0    0    0    0    1    0      0      0      0      0      0      -0.1        >=  0
      
      
       <unnamed> | x == ifelse(1:n <= 4, 2, 8)
      
       x[1] x[2] x[3] x[4] x[5] x[6] cap[1] cap[2] cap[3] cap[4] cap[5] cap[6] cap_special dir  
       1    0    0    0    0    0    0      0      0      0      0      0      0           ==  2
       0    1    0    0    0    0    0      0      0      0      0      0      0           ==  2
       0    0    1    0    0    0    0      0      0      0      0      0      0           ==  2
       0    0    0    1    0    0    0      0      0      0      0      0      0           ==  2
       0    0    0    0    1    0    0      0      0      0      0      0      0           ==  8
       0    0    0    0    0    1    0      0      0      0      0      0      0           ==  8
      

