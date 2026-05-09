# objective

    Code
      lp_maximize(p, -x)$objective
    Output
      maximize -x 
      
      $coef
           x   y[a]   y[b]   y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c] 
          -1      0      0      0      0      0      0      0      0      0 
      

---

    Code
      lp_minimize(p, sum(y))$objective
    Output
      minimize sum(y) 
      
      $coef
           x   y[a]   y[b]   y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c] 
           0      1      1      1      0      0      0      0      0      0 
      

---

    Code
      lp_minimize(p, {
        i <- 1:2
        j <- 1:3
        z[i, j] * outer(i, j, `^`)
      })$objective
    Message
      Summing variables in objective. Write `sum(...)` to suppress this message.
    Output
      minimize {     i <- 1:2     j <- 1:3     z[i, j] * outer(i, j, `^`) } 
      
      $coef
           x   y[a]   y[b]   y[c] z[1,a] z[2,a] z[1,b] z[2,b] z[1,c] z[2,c] 
           0      0      0      0      1      2      1      4      1      8 
      

