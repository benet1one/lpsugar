# new alias

    Code
      unclass(p$aliases$y)
    Output
      $binary
      [1] FALSE
      
      $ind
      A
      a b c 
      1 2 3 
      with class 'robust_index' from package 'lpsugar'
      
      $L
           x[a,A] x[b,A] x[c,A] x[a,B] x[b,B] x[c,B]
      [1,]      1      0      0      1      0      0
      [2,]      0      1      0      0      1      0
      [3,]      0      0      0      0      0      0
      with class 'robust_index' from package 'lpsugar'
      
      $A
           [,1]
      [1,]    0
      [2,]    0
      [3,]    5
      with class 'robust_index' from package 'lpsugar'
      

---

    Code
      unclass(p2$aliases$z)
    Output
      $binary
      [1] FALSE
      
      $ind
         A
      A   a b c
        a 1 4 7
        b 2 5 8
        c 3 6 9
      with class 'robust_index' from package 'lpsugar'
      
      $L
            x[a,a] x[b,a] x[c,a] x[a,b] x[b,b] x[c,b] x[a,c] x[b,c] x[c,c]
       [1,]      1      0      0      0      0      0      0      0      0
       [2,]      1      0      0      0      0      0      0      0      0
       [3,]      1      0      0      0      0      0      0      0      0
       [4,]      0      0      0      0      1      0      0      0      0
       [5,]      0      0      0      0      1      0      0      0      0
       [6,]      0      0      0      0      1      0      0      0      0
       [7,]      0      0      0      0      0      0      0      0      1
       [8,]      0      0      0      0      0      0      0      0      1
       [9,]      0      0      0      0      0      0      0      0      1
      with class 'robust_index' from package 'lpsugar'
      
      $A
            [,1]
       [1,]    2
       [2,]    2
       [3,]    2
       [4,]    2
       [5,]    2
       [6,]    2
       [7,]    2
       [8,]    2
       [9,]    2
      with class 'robust_index' from package 'lpsugar'
      

---

    Code
      unclass(p3$aliases$y)
    Output
      $binary
      [1] FALSE
      
      $ind
      A
      a b c 
      1 2 3 
      with class 'robust_index' from package 'lpsugar'
      
      $L
           x[a] x[b] x[c]
      [1,]    2    0    0
      [2,]    0    2    0
      [3,]    0    0    0
      with class 'robust_index' from package 'lpsugar'
      
      $A
           [,1]
      [1,]    0
      [2,]    0
      [3,]    0
      with class 'robust_index' from package 'lpsugar'
      
      $Q
      $Q[[1]]
           x[a] x[b] x[c]
      x[a]    0    0    0
      x[b]    0    0    0
      x[c]    0    0    0
      
      $Q[[2]]
           x[a] x[b] x[c]
      x[a]    0    0    0
      x[b]    0    0    0
      x[c]    0    0    0
      
      $Q[[3]]
           x[a] x[b] x[c]
      x[a]    2    2    2
      x[b]    2    2    2
      x[c]    2    2    2
      with class 'robust_index' from package 'lpsugar'
      
      

