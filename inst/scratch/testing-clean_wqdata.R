library(rems)
devtools::load_all(".")

foo <- get_ems_data()
filtered_data <- filter_ems_data(foo, emsid = "0121580")

tidy_filtered_data <- tidy_ems_data(filtered_data)

clean_filtered_data <- clean_wqdata(tidy_filtered_data, by = c("EMS_ID", "Station"))

calc_limits(clean_filtered_data, term = "short")
