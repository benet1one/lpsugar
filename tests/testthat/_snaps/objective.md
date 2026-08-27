# objective

    Code
      lp_maximize(p, -x)$objective
    Output
      linear function:
      -x
      

---

    Code
      lp_minimize(p, sum(y))$objective
    Output
      linear function:
      sum(y)
      

---

    Code
      unclass(p2$objective)
    Output
      $L
       [1] 0 0 0 0 0 0 1 0 0 0
      
      $names
       [1] "x"      "y[a]"   "y[b]"   "y[c]"   "z[1,a]" "z[2,a]" "z[1,b]" "z[2,b]"
       [9] "z[1,c]" "z[2,c]"
      
      attr(,"nobj")
      [1] 10
      attr(,"lpsugar_attributes")
      attr(,"lpsugar_attributes")$A
      [1] 0
      
      attr(,"lpsugar_attributes")$type
      [1] "linear"
      
      attr(,"lpsugar_attributes")$expr
      [1] "{ ... }"
      

# quadratic objective

    Code
      p$objective
    Output
      quadratic function:
      sum(x^2) + sum(y)
      

---

    Code
      unclass(p$objective)
    Output
      $Q
           x[1] x[2] y[1] y[2]
      x[1]    2    0    0    0
      x[2]    0    2    0    0
      y[1]    0    0    0    0
      y[2]    0    0    0    0
      
      $L
      [1] 0 0 1 1
      
      $names
      [1] "x[1]" "x[2]" "y[1]" "y[2]"
      
      attr(,"nobj")
      [1] 4
      attr(,"lpsugar_attributes")
      attr(,"lpsugar_attributes")$A
      [1] 0
      
      attr(,"lpsugar_attributes")$type
      [1] "quadratic"
      
      attr(,"lpsugar_attributes")$expr
      [1] "sum(x^2) + sum(y)"
      

# update objective

    Code
      unclass(p$objective)
    Output
      $Q
           x y z[1] z[2]
      x    2 5    0    0
      y    5 0    0    0
      z[1] 0 0    0    0
      z[2] 0 0    0    0
      
      $L
      [1] 0 3 0 0
      
      $names
      [1] "x"    "y"    "z[1]" "z[2]"
      
      attr(,"nobj")
      [1] 2
      attr(,"lpsugar_attributes")
      attr(,"lpsugar_attributes")$A
      [1] 1
      
      attr(,"lpsugar_attributes")$type
      [1] "quadratic"
      
      attr(,"lpsugar_attributes")$expr
      [1] "x^2 + 5 * x * y + 3 * y + 1"
      

