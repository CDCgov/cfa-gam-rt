# predict can handle string dates

    Code
      actual <- predict(fit, min_date = "2023-01-05", max_date = "2023-01-10")
    Message
      > Casting `min_date` a string `2023-01-05` to a <Date> object "2023-01-05"
      > Casting `max_date` a string `2023-01-10` to a <Date> object "2023-01-10"

# Bad dates throw appropriate status messages

    Code
      actual <- parse_predict_dates(list(), min_date = max_date, max_date = min_date)
    Message
      ! Swapping `min_date` and `max_date`
      > `min_date` 2023-01-02 is after `max_date` 2023-01-01

