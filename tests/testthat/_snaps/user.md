# solution summary

    Code
      solution_summary(p, slist)
    Output
      $aliases
      $aliases$x1
      [1] 50
      
      
      $constraints
         name fullname        lhs dir rhs satisfied saturated
      1    c1  c1[i=1] 50.0000000  <=  10     FALSE        NA
      2    c1  c1[i=2]  1.0000000  <=  10      TRUE     FALSE
      3    c1  c1[i=3] 10.0000000  <=  10      TRUE      TRUE
      4    c1  c1[i=4]  1.0000000  <=  10      TRUE     FALSE
      5    c1  c1[i=5] 10.0000000  <=  10      TRUE      TRUE
      6    c1  c1[i=6] -5.0000000  <=  10      TRUE     FALSE
      7    c2       c2 16.6666667  <=  50      TRUE     FALSE
      8    c2       c2  0.3333333  <=  50      TRUE     FALSE
      9    c2       c2  3.3333333  <=  50      TRUE     FALSE
      10   c2       c2  0.3333333  <=  50      TRUE     FALSE
      11   c2       c2  3.3333333  <=  50      TRUE     FALSE
      12   c2       c2 -1.6666667  <=  50      TRUE     FALSE
      13               51.0000000  ==   4     FALSE        NA
      
      $bounds
        variable lower value upper satisfied saturated
      1        y  -Inf     0   Inf      TRUE     FALSE
      2   x[1,1]     1    50   Inf      TRUE     FALSE
      3   x[2,1]     1     1   Inf      TRUE      TRUE
      4   x[1,2]     1    10   Inf      TRUE     FALSE
      5   x[2,2]     1     1   Inf      TRUE      TRUE
      6   x[1,3]     1    10   Inf      TRUE     FALSE
      7   x[2,3]     1    -5   Inf     FALSE        NA
      
      $feasible
      [1] FALSE
      
      $objective
      [1] 67
      

