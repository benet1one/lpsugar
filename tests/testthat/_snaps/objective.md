# objective snaps

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
      

