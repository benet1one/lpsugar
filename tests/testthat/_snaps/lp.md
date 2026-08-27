# printing

    Code
      p
    Output
      ---- <lp_problem> ----
      
      -- $variables
      $x
      Real scalar 'x'
      x >= 0
      
      $y
      Integer scalar 'y'
      y >= 0
      
      -- $objective
      maximize linear function:
      x + y
      
      -- $constraints
      An object containing 2 linear constraints.
      
      #unnamed_constraint
      | x + 2 * y <= 10
      | Rows = 1
      
      #unnamed_constraint
      | 2 * x + y <= 10
      | Rows = 1
      

---

    Code
      s
    Output
      -- $variables
      $x
      [1] 3.5
      
      $y
      [1] 3
      
      -- $aliases
      $two_x
      [1] 7
      
      -- $objective
      [1] 6.5
      
      -- $status
      Optimal Solution Found [v] 
      

---

    Code
      plong
    Output
      ---- <lp_problem> ----
      
      -- $variables
      $x
      Real variable 'x[1:1000]'
      
      -- $objective
      minimize linear function:
      x[2]
      
      -- $constraints
      An object containing 6 linear constraints.
      
      #unnamed_constraint
      | for (i in 1:2) for (j in 1:3) i + j <= x[i * j]
      | Rows = 6
      

# feasible

    Code
      p
    Output
      ---- <lp_problem> ----
      
      -- $variables
      $x
      Real scalar 'x'
      5 <= x <= 10
      
      -- $objective
      find a feasible solution
      

# infeasible

    Code
      s
    Output
      -- $variables
      $z
      [1] 0 0 0
      
      -- $aliases
      $a
      [1] 0
      
      -- $objective
      [1] 0
      
      -- $status
      No Optimal Solution Found [x] 
      
      $code
      [1] 1
      
      $msg
        solver highs
          code 8
        symbol INFEASIBLE
       message Infeasible
      roi_code 1
      

