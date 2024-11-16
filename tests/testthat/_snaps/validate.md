# `validate_dates()` is successful

    Code
      validate_dates(not_a_vector, "test")
    Condition
      Error in `throw_type_error()`:
      ! `dates` is a list
      i Must be of type Date

# validate_predict_inputs handles 'obs_cases' correctly

    Code
      validate_predict_inputs(parameter = "obs_cases", mean_delay = 2, gi_pmf = c(0.5,
        0.5))
    Message
      > `mean_delay` ignored when `parameter` is "obs_cases"
      > `gi_pmf` ignored when `parameter` is "obs_cases"

---

    Code
      validate_predict_inputs(parameter = "obs_cases", mean_delay = NULL, gi_pmf = c(
        0.5, 0.5))
    Message
      > `gi_pmf` ignored when `parameter` is "obs_cases"

# validate_predict_inputs requires mean_delay for non-obs_cases parameters

    Code
      validate_predict_inputs(parameter = "r", mean_delay = NULL, gi_pmf = c(0.5, 0.5))
    Condition
      Error:
      ! `parameter` "r" requires `mean_delay`

# validate_predict_inputs requires gi_pmf for parameter 'Rt'

    Code
      expect_error(validate_predict_inputs(parameter = "Rt", mean_delay = 2, gi_pmf = NULL))

# validate_predict_inputs checks gi_pmf values for 'Rt'

    Code
      validate_predict_inputs(parameter = "Rt", mean_delay = 2, gi_pmf = c(0.5, NA))
    Condition
      Error:
      ! `gi_pmf` has missing values
      i Missing values are not supported in `gi_pmf`
      ! Missing element(s) index: 2

---

    Code
      validate_predict_inputs(parameter = "Rt", mean_delay = 2, gi_pmf = c(-0.1, 1.1))
    Condition
      Error:
      ! `gi_pmf` has elements smaller than 0
      ! All elements must be 0 or greater
      i Elements 1 are smaller

# validate_predict_inputs handles invalid parameter values

    Code
      validate_predict_inputs(parameter = "unknown", mean_delay = 2, gi_pmf = c(0.5,
        0.5))
    Condition
      Error:
      ! `parameter` must be one of "obs_cases", "r", or "Rt", not "unknown".

