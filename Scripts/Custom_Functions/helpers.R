# helpers.R

# zero_pad_2() -----

zero_pad_2_across <- function() {
  ~ sql(
    paste0(
      "lpad(NULLIF(trim(",
      cur_column(),
      "), ''), 2, '0')"
    )
  )
}
