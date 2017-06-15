library(rems)
devtools::load_all(".")

foo <- get_ems_data()
filtered_data <- filter_ems_data(foo, emsid = "E307225",
from_date = "2017-01-01",
to_date = "2017-04-30")

tidy_filtered_data <- tidy_ems_data(filtered_data)

clean_wqdata(tidy_filtered_data)
