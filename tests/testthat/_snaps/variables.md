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
      attr(,"class")
      [1] "robust_index" "matrix"       "array"       
      
      $add
           [,1]
      [1,]    0
      [2,]    0
      [3,]    0
      [4,]    0
      [5,]    0
      [6,]    0
      attr(,"class")
      [1] "robust_index" "matrix"       "array"       
      

