# Bad dates throw appropriate status messages

    Code
      actual <- parse_predict_dates(list(), min_date = max_date, max_date = min_date)
    Message
      ! Swapping `min_date` and `max_date`
      > `min_date` 2023-01-02 is after `max_date` 2023-01-01

