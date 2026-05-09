# objective

    Code
      lp_maximize(p, -x)$objective
    Output
      maximize linear function:
      -x
      

---

    Code
      lp_minimize(p, sum(y))$objective
    Output
      minimize linear function:
      sum(y)
      

---

    Code
      unclass(lp_minimize(p, {
        i <- 1
        j <- a[2]
        z[i, j]
      })$objective)
    Output
      $coef
           x   y[a]   y[b]   y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c] 
           0      0      0      0      0      0      1      0      0      0 
      
      $add
      [1] 0
      
      $direction
      [1] "minimize"
      
      $expr
      [1] "{ ... }"
      

# quadratic objective

    Code
      p$objective
    Output
      minimize quadratic function:
      sum(x^2) + sum(y)
      

---

    Code
      unclass(p$objective)
    Output
      $coef
      x[1] x[2] y[1] y[2] 
         0    0    1    1 
      
      $add
      [1] 0
      
      $direction
      [1] "minimize"
      
      $expr
      [1] "sum(x^2) + sum(y)"
      
      $q_coef
           x[1] x[2] y[1] y[2]
      x[1]    2    0    0    0
      x[2]    0    2    0    0
      y[1]    0    0    0    0
      y[2]    0    0    0    0
      with class 'robust_index' from package 'lpsugar'
      

