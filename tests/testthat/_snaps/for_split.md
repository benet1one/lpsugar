# for_split works

    Code
      for_split(q1)
    Output
      $`i=1`
      [1] 1
      
      $`i=2`
      [1] 4
      
      $`i=3`
      [1] 9
      

---

    Code
      for_split(q2, data = data_mask(p))
    Output
      $`i=1, j="a"`
      $coef
           x[1,a] x[2,a] x[1,b] x[2,b] x[1,c] x[2,c]
      [1,]      1      0      0      0      0      0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    1
      with class 'robust_index' from package 'lpsugar'
      
      
      $`i=1, j="b"`
      $coef
           x[1,a] x[2,a] x[1,b] x[2,b] x[1,c] x[2,c]
      [1,]      0      0      1      0      0      0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    1
      with class 'robust_index' from package 'lpsugar'
      
      
      $`i=1, j="c"`
      $coef
           x[1,a] x[2,a] x[1,b] x[2,b] x[1,c] x[2,c]
      [1,]      0      0      0      0      1      0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    1
      with class 'robust_index' from package 'lpsugar'
      
      
      $`i=2, j="a"`
      $coef
           x[1,a] x[2,a] x[1,b] x[2,b] x[1,c] x[2,c]
      [1,]      0      1      0      0      0      0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    2
      with class 'robust_index' from package 'lpsugar'
      
      
      $`i=2, j="b"`
      $coef
           x[1,a] x[2,a] x[1,b] x[2,b] x[1,c] x[2,c]
      [1,]      0      0      0      1      0      0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    2
      with class 'robust_index' from package 'lpsugar'
      
      
      $`i=2, j="c"`
      $coef
           x[1,a] x[2,a] x[1,b] x[2,b] x[1,c] x[2,c]
      [1,]      0      0      0      0      0      1
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    2
      with class 'robust_index' from package 'lpsugar'
      
      

# advanced for_split

    Code
      for_split(q_advanced)
    Output
      $`i=1, j=2`
      i j k 
      1 2 2 
      
      $`i=1, j=3`
      i j k 
      1 3 2 
      
      $`i=2, j=3`
      NULL
      
      $`i=2, j=4`
      NULL
      
      $`i=3, j=4`
      i j k 
      3 4 4 
      
      $`i=3, j=5`
      i j k 
      3 5 4 
      
      $`i=4, j=5`
      NULL
      
      $`i=4, j=6`
      NULL
      

