test_that("errors correctly", {
  chk::expect_chk_error(summarise_wqdata(data.frame(Value = 1)))
  chk::expect_chk_error(summarise_wqdata(data.frame(Variable = 1, Value = 1)))
  chk::expect_chk_error(summarise_wqdata(data.frame(Variable = "1", Value = 1), censored = TRUE))
})

test_that("zero rows", {
  x <- summarise_wqdata(data.frame(Variable = character(0), Value = double(0), stringsAsFactors = FALSE))
  y <-  tibble::tibble(
    Variable = character(0),
    n = integer(0),
    ncen = integer(0),
    min = double(0),
    max = double(0),
    mean = double(0),
    median = double(0),
    lowerQ = double(0),
    upperQ = double(0),
    sd = double(0),
    se = double(0),
    lowerCL = double(0),
    upperCL = double(0))

  expect_identical(x,y)
})

test_that("zero rows with integer values", {
  x <- summarise_wqdata(data.frame(Variable = character(0), Value = integer(0), stringsAsFactors = FALSE))
  y <-  tibble::tibble(
    Variable = character(0),
    n = integer(0),
    ncen = integer(0),
    min = integer(0),
    max = integer(0),
    mean = double(0),
    median = double(0),
    lowerQ = double(0),
    upperQ = double(0),
    sd = double(0),
    se = double(0),
    lowerCL = double(0),
    upperCL = double(0))

  expect_identical(x,y)
})

test_that("works", {
  set.seed(99)
  x <- summarise_wqdata(data.frame(Variable = "1", Value = rlnorm(1000), stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 0L, min = 0.0455637968166263,
    max = 36.359661036231, mean = 1.64623322183978, median = 0.976190223959382,
    lowerQ = 0.489850822752585, upperQ = 1.94538277592153, sd = 2.23542268426457,
    se = 0.070690272154835, lowerCL = 1.52244306862086, upperCL = 1.78008877740433)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("all missing values", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = NA_real_, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "1", n = 1L, ncen = 0L, min = NA_real_,
    max = NA_real_, mean = NA_real_, median = NA_real_, lowerQ = NA_real_,
    upperQ = NA_real_, sd = NA_real_, se = NA_real_, lowerCL = NA_real_,
    upperCL = NA_real_)
  expect_identical(x, y)
})

test_that("all missing values censored", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = NA_real_, DetectionLimit = 1, stringsAsFactors = FALSE), censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 1L, ncen = NA_integer_, min = NA_real_,
    max = NA_real_, mean = NA_real_, median = NA_real_, lowerQ = NA_real_,
    upperQ = NA_real_, sd = NA_real_, se = NA_real_, lowerCL = NA_real_,
    upperCL = NA_real_)
  expect_identical(x, y)
})

test_that("some missing values all censored", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = c(1, NA_real_), DetectionLimit = 1, stringsAsFactors = FALSE), censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 2L, ncen = NA_integer_, min = NA_real_,
    max = NA_real_, mean = NA_real_, median = NA_real_, lowerQ = NA_real_,
    upperQ = NA_real_, sd = NA_real_, se = NA_real_, lowerCL = NA_real_,
    upperCL = NA_real_)
  expect_identical(x, y)
})

test_that("some missing values only non-missing censored", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = c(1, NA_real_), DetectionLimit = c(1, NA_real_), stringsAsFactors = FALSE), censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 2L, ncen = 1L, min = NA_real_,
    max = NA_real_, mean = NA_real_, median = NA_real_, lowerQ = NA_real_,
    upperQ = NA_real_, sd = NA_real_, se = NA_real_, lowerCL = NA_real_,
    upperCL = NA_real_)
  expect_identical(x, y)
})

test_that("some missing values uncensored", {
  x <- summarise_wqdata(data.frame(Variable = "1", Value = c(NA_real_, 1), stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "1", n = 2L, ncen = 0L, min = NA_real_,
    max = NA_real_, mean = NA_real_, median = NA_real_, lowerQ = NA_real_,
    upperQ = NA_real_, sd = NA_real_, se = NA_real_, lowerCL = NA_real_,
    upperCL = NA_real_)
  expect_identical(x, y)
})

test_that("na.rm = TRUE with only 1 non-missing value", {
  x <- suppressWarnings(
    summarise_wqdata(
      data.frame(
        Variable = "1",
        Value = c(NA_real_, 1),
        stringsAsFactors = FALSE
      ),
      na.rm = TRUE
    )
  )
  expect_warning(
    summarise_wqdata(
      data.frame(
        Variable = "1",
        Value = c(NA_real_, 1),
        stringsAsFactors = FALSE
      ),
      na.rm = TRUE
    )
  )
  y <- tibble::tibble(Variable = "1", n = 1L, ncen = 0L, min = 1, max = 1,
    mean = NA_real_, median = NA_real_, lowerQ = NA_real_, upperQ = NA_real_,
    sd = NA_real_, se = NA_real_, lowerCL = NA_real_, upperCL = NA_real_)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("multiple variables each with 1 value", {
  expect_warning(
    summarise_wqdata(
      data.frame(
        Variable = c("1", "three"), Value = c(1, 3), stringsAsFactors = FALSE
      )
    )
  )

  x <- suppressWarnings(
    summarise_wqdata(
      data.frame(
        Variable = c("1", "three"), Value = c(1, 3), stringsAsFactors = FALSE
      )
    )
  )

  y <- tibble::tibble(
    Variable = c("1", "three"), n = c(1L, 1L), ncen = c(0L, 0L), min = c(1, 3),
    max = c(1, 3), mean = c(NA_real_, NA_real_), median = c(NA_real_, NA_real_),
    lowerQ = c(NA_real_, NA_real_), upperQ = c(NA_real_, NA_real_),
    sd = c(NA_real_, NA_real_), se = c(NA_real_, NA_real_),
    lowerCL = c(NA_real_, NA_real_), upperCL = c(NA_real_, NA_real_))

  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("two integer values", {
  x <- summarise_wqdata(data.frame(Variable = "var", Value = 1:2, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = "var", n = 2L, ncen = 0L, min = 1L,
    max = 2L, mean = 1.5017486818551, median = 1.41421356237309,
    lowerQ = 1.11942286789001, upperQ = 1.78663493248962, sd = 0.53649338845922,
    se = 0.379358113041263, lowerCL = 0.920159272574001, upperCL = 2.45093340976159)

  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("missing values in variables", {
  x <- summarise_wqdata(data.frame(Variable = NA_character_, Value = 1:2, stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = NA_character_, n = 2L, ncen = 0L, min = 1L,
    max = 2L, mean = 1.5017486818551, median = 1.41421356237309,
    lowerQ = 1.11942286789001, upperQ = 1.78663493248962, sd = 0.53649338845922,
    se = 0.379358113041263, lowerCL = 0.920159272574001, upperCL = 2.45093340976159)

  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("factor variable", {
  x <- summarise_wqdata(data.frame(Variable = factor("1"), Value = c(1,2), stringsAsFactors = FALSE))
  y <- tibble::tibble(Variable = factor("1"), n = 2L, ncen = 0L, min = 1,
    max = 2, mean = 1.5017486818551, median = 1.41421356237309,
    lowerQ = 1.11942286789001, upperQ = 1.78663493248962, sd = 0.53649338845922,
    se = 0.379358113041263, lowerCL = 0.920159272574001, upperCL = 2.45093340976159)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  x <- summarise_wqdata(data, censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 260L, min = 0.0455637968166263,
    max = 36.359661036231, mean = 1.65082950042163, median = 0.969085874824302,
    lowerQ = 0.483074099197225, upperQ = 1.94406496714402, sd = 2.27663879655169,
    se = 0.0719936400660803, lowerCL = 1.52514916805004, upperCL = 1.78686655479519)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower 0s censored", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  data$Value[data$Value <= data$DetectionLimit] <- 0

  x <- summarise_wqdata(data, censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 260L, min = 0,
    max = 36.359661036231, mean = 1.65082950042163, median = 0.969085874824302,
    lowerQ = 0.483074099197225, upperQ = 1.94406496714402, sd = 2.27663879655169,
    se = 0.0719936400660803, lowerCL = 1.52514916805004, upperCL = 1.78686655479519)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("0s", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)
  data$Value[data$Value <= data$DetectionLimit] <- 0

  x <- summarise_wqdata(data)

  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 0L, min = 0,
    max = 36.359661036231, mean = NA_real_, median = NA_real_,
    lowerQ = NA_real_, upperQ = NA_real_, sd = NA_real_,
    se = NA_real_, lowerCL = NA_real_, upperCL = NA_real_)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored not quite 0", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)
  data$Value[data$Value <= data$DetectionLimit] <- 0.001

  x <- summarise_wqdata(data)

  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 0L, min = 0.001,
    max = 36.359661036231, mean = 48.8749791701325, median = 0.226523789098847,
    lowerQ = 0.0248177221015859, upperQ = 2.06759616445297,
    sd = 10545.1967836686, se = 333.468402110748, lowerCL = 29.2671569135385,
    upperCL = 81.6192565590778)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored conf_level", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  x <- summarise_wqdata(data, censored = TRUE, conf_level = 0.8)
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 260L, min = 0.0455637968166263,
    max = 36.359661036231, mean = 1.65082950042163, median = 0.969085874824302,
    lowerQ = 0.483074099197225, upperQ = 1.94406496714402, sd = 2.27663879655169,
    se = 0.0719936400660803, lowerCL = 1.56753004900058, upperCL = 1.73855553276308)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored quan_range", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  x <- summarise_wqdata(data, censored = TRUE, quan_range = 0.95)
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 260L, min = 0.0455637968166263,
    max = 36.359661036231, mean = 1.65082950042163, median = 0.969085874824302,
    lowerQ = 0.128169227038409, upperQ = 7.32724581776991, sd = 2.27663879655169,
    se = 0.0719936400660803, lowerCL = 1.52514916805004, upperCL = 1.78686655479519)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored missing value", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  data$Value[1] <- NA_real_

  x <- summarise_wqdata(data, censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = NA_integer_,
    min = NA_real_, max = NA_real_, mean = NA_real_, median = NA_real_,
    lowerQ = NA_real_, upperQ = NA_real_, sd = NA_real_, se = NA_real_,
    lowerCL = NA_real_, upperCL = NA_real_)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored missing value na.rm = TRUE", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  data$Value[1] <- NA_real_

  x <- summarise_wqdata(data, censored = TRUE, na.rm = TRUE)
  y <- tibble::tibble(Variable = "1", n = 999L, ncen = 260L, min = 0.0455637968166263,
    max = 36.359661036231, mean = 1.65145740813986, median = 0.968670543090355,
    lowerQ = 0.482611967837587, upperQ = 1.94425891520108, sd = 2.2803164138024,
    se = 0.0721460185668738, lowerCL = 1.52554520191114, upperCL = 1.78776188832907)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored all detection limits missing", {
  set.seed(99)
  data <- data.frame(Variable = "1", Value = rlnorm(1000),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  data$DetectionLimit <- NA_real_

  x <- summarise_wqdata(data, censored = TRUE)
  y <- tibble::tibble(Variable = "1", n = 1000L, ncen = 0L, min = 0.0455637968166263,
    max = 36.359661036231, mean = 1.64623322183978, median = 0.976190223959382,
    lowerQ = 0.489850822752585, upperQ = 1.94538277592153, sd = 2.23542268426457,
    se = 0.070690272154835, lowerCL = 1.52244306862086, upperCL = 1.78008877740433)
  expect_equal(as.data.frame(x), as.data.frame(y))
})

test_that("lower censored by ", {
  set.seed(99)
  data1 <- data.frame(Variable = "1", MeanLog = 0, Value = rlnorm(1000, meanlog = 1),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)
  data2 <- data.frame(Variable = "1", MeanLog = 2, Value = rlnorm(1000, meanlog = 2),
                                   DetectionLimit = 0.5, stringsAsFactors = FALSE)

  data <- rbind(data1, data2)

  x <- summarise_wqdata(data, by = "MeanLog", censored = TRUE)

  y <- tibble::tibble(Variable = c("1", "1"), MeanLog = c(0, 2), n = c(1000L,
1000L), ncen = c(54L, 3L), min = c(0.123855240922235, 0.336010163606413
), max = c(98.8358058837172, 165.557501183876), mean = c(4.48406426490208,
12.1971429147508), median = c(2.6503014491015, 7.56195983066146
), lowerQ = c(1.32705678555795, 3.91023362790236), upperQ = c(5.29298960492958,
14.6239948612005), sd = c(6.11963921796074, 15.436214422828),
    se = c(0.193519983872475, 0.488135960268779), lowerCL = c(4.14551983124041,
    11.3308870601644), upperCL = c(4.8502559751971, 13.1296247586724
    ))
  expect_equal(as.data.frame(x), as.data.frame(y))
})
