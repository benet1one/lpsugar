# printing

    Code
      p
    Output
      <lpsugar Linear Problem>
      
      -- $variables --
      $x
      Real scalar 'x'
      x >= 0
      
      $y
      Integer scalar 'y'
      y >= 0
      
      -- $objective --
      maximize x + y 
      $coef
      x y 
      1 1 
      
      -- $constraints --
       <unnamed> | x + 2 * y <= 10
       <unnamed> | 2 * x + y <= 10

---

    Code
      s
    Output
      $variables
      $variables$x
      [1] 3.5
      
      $variables$y
      [1] 3
      
      
      $objective
      [1] 6.5
      
      $status$code = 0 : Optimal
      
      Fields:
      -- $objective --
      -- $variables --
      -- $aliases --
      -- $variables_vec --
      -- $status --
      -- $message --
      -- $model --

