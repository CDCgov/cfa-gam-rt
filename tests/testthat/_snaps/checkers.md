# Type-checking dates works

    Code
      check_date(not_dates, arg, call)
    Condition
      Error:
      ! `test` "abc" and "123" could not be automatically cast to date.
      Try explicitly converting with `as.Date()`

# Autofixing dates works as expected

    Code
      autofixed <- check_date(date_string, "potato")
    Message
      > Casting `potato` a string `2023-01-01` to a <Date> object "2023-01-01"

