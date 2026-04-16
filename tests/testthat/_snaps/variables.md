# variable definitions

    Code
      parse_variable_definition(t[a, b = 1:5])
    Output
      $name
      [1] "t"
      
      $sets
      $sets$a
      [1] "a" "b" "c"
      
      $sets$b
      [1] 1 2 3 4 5
      
      

---

    Code
      parse_variable_definition(t[b = a, 1:5])
    Output
      $name
      [1] "t"
      
      $sets
      $sets$b
      [1] "a" "b" "c"
      
      $sets$`1:5`
      [1] 1 2 3 4 5
      
      

# variable concatenation

    Code
      bind_vars(1:2, y, x[1], 3)
    Output
      $coef
           x[1] x[2] x[3] y[1,1] y[2,1] y[1,2] y[2,2]
      [1,]    0    0    0      0      0      0      0
      [2,]    0    0    0      0      0      0      0
      [3,]    0    0    0      1      0      0      0
      [4,]    0    0    0      0      1      0      0
      [5,]    0    0    0      0      0      1      0
      [6,]    0    0    0      0      0      0      1
      [7,]    1    0    0      0      0      0      0
      [8,]    0    0    0      0      0      0      0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    1
      [2,]    2
      [3,]    0
      [4,]    0
      [5,]    0
      [6,]    0
      [7,]    0
      [8,]    3
      with class 'robust_index' from package 'lpsugar'
      

# variable indexing

    Code
      t(z)
    Output
      $coef
           x y[a] y[b] y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c]
      [1,] 0    0    0    0      1      0      0      0      0      0
      [2,] 0    0    0    0      0      0      1      0      0      0
      [3,] 0    0    0    0      0      0      0      0      1      0
      [4,] 0    0    0    0      0      1      0      0      0      0
      [5,] 0    0    0    0      0      0      0      1      0      0
      [6,] 0    0    0    0      0      0      0      0      0      1
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    0
      [2,]    0
      [3,]    0
      [4,]    0
      [5,]    0
      [6,]    0
      with class 'robust_index' from package 'lpsugar'
      

---

    Code
      t(ThreeD[, , 1, drop = TRUE])
    Output
      $coef
           ThreeD[1,1,1] ThreeD[2,1,1] ThreeD[1,2,1] ThreeD[2,2,1] ThreeD[1,3,1]
      [1,]             1             0             0             0             0
      [2,]             0             0             1             0             0
      [3,]             0             0             0             0             1
      [4,]             0             1             0             0             0
      [5,]             0             0             0             1             0
      [6,]             0             0             0             0             0
           ThreeD[2,3,1] ThreeD[1,1,2] ThreeD[2,1,2] ThreeD[1,2,2] ThreeD[2,2,2]
      [1,]             0             0             0             0             0
      [2,]             0             0             0             0             0
      [3,]             0             0             0             0             0
      [4,]             0             0             0             0             0
      [5,]             0             0             0             0             0
      [6,]             1             0             0             0             0
           ThreeD[1,3,2] ThreeD[2,3,2] ThreeD[1,1,3] ThreeD[2,1,3] ThreeD[1,2,3]
      [1,]             0             0             0             0             0
      [2,]             0             0             0             0             0
      [3,]             0             0             0             0             0
      [4,]             0             0             0             0             0
      [5,]             0             0             0             0             0
      [6,]             0             0             0             0             0
           ThreeD[2,2,3] ThreeD[1,3,3] ThreeD[2,3,3]
      [1,]             0             0             0
      [2,]             0             0             0
      [3,]             0             0             0
      [4,]             0             0             0
      [5,]             0             0             0
      [6,]             0             0             0
      with class 'robust_index' from package 'lpsugar'
      
      $add
           [,1]
      [1,]    0
      [2,]    0
      [3,]    0
      [4,]    0
      [5,]    0
      [6,]    0
      with class 'robust_index' from package 'lpsugar'
      

# operations

    Code
      diff(y)$coef
    Output
           x y[a] y[b] y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c]
      [1,] 0   -1    1    0      0      0      0      0      0      0
      [2,] 0    0   -1    1      0      0      0      0      0      0
      with class 'robust_index' from package 'lpsugar'

---

    Code
      diff(y, lag = 2)$coef
    Output
           x y[a] y[b] y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c]
      [1,] 0   -1    0    1      0      0      0      0      0      0
      with class 'robust_index' from package 'lpsugar'

