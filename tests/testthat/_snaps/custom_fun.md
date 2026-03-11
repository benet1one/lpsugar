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
      

