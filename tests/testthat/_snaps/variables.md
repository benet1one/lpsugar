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
      
      

