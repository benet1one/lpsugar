# new impvar

    Code
      unclass(p$impvars$y)
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
      unclass(p2$impvars$z)
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
      

