# quadratic arrays

    Code
      z^c(0, 1, 2)
    Output
      $Q
      $Q[[1]]
           z[1] z[2] z[3]
      z[1]    0    0    0
      z[2]    0    0    0
      z[3]    0    0    0
      with class 'robust_index' from package 'lpsugar'
      
      $Q[[2]]
           z[1] z[2] z[3]
      z[1]    0    0    0
      z[2]    0    0    0
      z[3]    0    0    0
      with class 'robust_index' from package 'lpsugar'
      
      $Q[[3]]
           z[1] z[2] z[3]
      z[1]    0    0    0
      z[2]    0    0    0
      z[3]    0    0    2
      with class 'robust_index' from package 'lpsugar'
      
      
      $L
           z[1] z[2] z[3]
      [1,]    0    0    0
      [2,]    0    1    0
      [3,]    0    0    0
      with class 'robust_index' from package 'lpsugar'
      
      $A
           [,1]
      [1,]    1
      [2,]    0
      [3,]    0
      with class 'robust_index' from package 'lpsugar'
      

# ifelse quadratic

    Code
      purrr::map_if(p$constraints$Q, ~ !is.null(.x), ~ as.matrix(.x))
    Output
      [[1]]
           x[1] x[2] x[3] x[4]
      x[1]    2    0    0    0
      x[2]    0    0    0    0
      x[3]    0    0    0    0
      x[4]    0    0    0    0
      
      [[2]]
           x[1] x[2] x[3] x[4]
      x[1]    0    0    0    0
      x[2]    0    8    0    0
      x[3]    0    0    0    0
      x[4]    0    0    0    0
      
      [[3]]
      NULL
      
      [[4]]
           x[1] x[2] x[3] x[4]
      x[1]    0    0    0    0
      x[2]    0    0    0    0
      x[3]    0    0    0    0
      x[4]    0    0    0   32
      

# quadratic constraints

    Code
      p$constraints
    Output
      
      <unnamed> | n = 3 | quadratic | x^2 < 1
      <unnamed> | n = 1 | x[3] > 0
      
       x[1] x[2] x[3] dir  
       0    0    1    >=  0
      

