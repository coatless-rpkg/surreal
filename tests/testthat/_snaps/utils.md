# require_packages() names the packages that are missing

    Code
      require_packages("notapackage1")
    Condition
      Error in `require_packages()`:
      ! Required package not installed: notapackage1
      i Install with: `install.packages(c("notapackage1"))`
    Code
      require_packages(c("stats", "notapackage1", "notapackage2"))
    Condition
      Error in `require_packages()`:
      ! Required packages not installed: notapackage1 and notapackage2
      i Install with: `install.packages(c("notapackage1", "notapackage2"))`

