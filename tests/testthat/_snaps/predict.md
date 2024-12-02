# predict can handle string dates

    Code
      actual <- predict(fit, min_date = "2023-01-05", max_date = "2023-01-10")
    Message
      > Casting `min_date` a string `2023-01-05` to a <Date> object "2023-01-05"
      > Casting `max_date` a string `2023-01-10` to a <Date> object "2023-01-10"

# Dates are parsed correctly

    Code
      actual_min_max_dates <- parse_predict_dates(object = object, min_date = min_date,
        max_date = max_date)

# Bad dates throw appropriate status messages

    Code
      actual <- parse_predict_dates(list(), min_date = max_date, max_date = min_date)
    Message
      ! Swapping `min_date` and `max_date`
      > `min_date` 2023-01-02 is after `max_date` 2023-01-01

# Day of week throws correctly formatted errors

    Code
      extract_dow_for_predict(object = object, day_of_week = c("Holiday", "New level"),
      desired_dates = as.Date(c("2023-01-02", "2023-01-03")))
    Condition
      Error in `extract_dow_for_predict()`:
      ! `day_of_week` provided unknown level
      ! Known levels: Sun, Mon, and Holiday
      x Provided but unknown: "New level"

---

    Code
      extract_dow_for_predict(object = object, day_of_week = "Holiday",
        desired_dates = as.Date(c("2023-01-02", "2023-01-03")))
    Condition
      Error in `extract_dow_for_predict()`:
      ! `day_of_week` was provided a vector of the wrong length
      > Provide 2 values, for 2023-01-02 to 2023-01-03
      x Was provided 1 values instead

---

    Code
      extract_dow_for_predict(object = object, day_of_week = TRUE, desired_dates = as.Date(
        c("2023-01-04", "2023-01-05")))
    Condition
      Error in `extract_dow_for_predict()`:
      ! `day_of_week` required when using custom levels and new dates
      x 2023-01-04 and 2023-01-05 weren't in the call to `RtGam()`
      i Pass `predict()` `day_of_week` a vector of values to use
      > Provide 2 values, for 2023-01-04 to 2023-01-05

