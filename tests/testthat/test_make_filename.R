filename <- make_filename(2015)
expect_that(filename, is_identical_to("accident_2015.csv.bz2"))
