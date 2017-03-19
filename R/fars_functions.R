#'
#' Function \code{fars_read} loads input data file into memory and transforms it
#' into dplyr tibble.
#'
#' @param filename file name in string format
#'
#' @return tbl_df data frame
#'
#' If the input file is not found, function stops returning an error:
#' "Filename does not exist"
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
"fars_read"
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'
#' Function \code{make_filename} creates file name based on provided year.
#'
#' @param year Four-digit year, as integer or as a string (i.e. 2001, "2001").
#'
#' @return file name string, such as "accident_YEAR.csv.bz2"
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename("2013")
#' }
"make_filename"
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#'
#' Function \code{fars_read_years} take as an argument a four-digit year and
#' makes a dataset containing columns month and year of an accident that occurred
#'
#' @inheritParams make_filename
#'
#' @return Data frame with columns:
#' \describe{
#'    \item{MONTH}{a value between 1 and 12, 1 for January, 12 for December}
#'    \item{year}{four-digit value for a year}
#' }
#'
#' If the requested year input is not present in the data, the function returns
#' a warning: "invalid year".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df mutate select
#' @param years Four-digit year, as integer or as a string (i.e. 2001, "2001").
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years("2013")
#' fars_read_years(c(2013,2014,2016))
#' }
"fars_read_years"
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                filename <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                        dplyr::select_(~ MONTH, ~year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#'
#' Function \code{fars_summarize_years} provides number of accidents for each
#' month and year.
#'
#' @inheritParams make_filename
#' @param years Four-digit year, as integer or as a string (i.e. 2001, "2001").
#'
#' @return Data frame with columns:
#' \describe{
#'    \item{MONTH}{a value between 1 and 12, 1 for January, 12 for December}
#'    \item{year}{four-digit value for a year, column contains numbers of
#'                accidents for each month for the given year}
#' }
#'
#' If the requested year input is not present in available data, the function stops
#' and return an error:
#' "file 'filename_YEAR.csv.bz2' does not exist".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df mutate select bind_rows group_by summarize spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years("2013")
#' }
"fars_summarize_years"
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_( ~ year, ~ MONTH) %>%
                dplyr::summarize_(n = ~ n()) %>%
                tidyr::spread_('year', 'n')
}

#'
#' Function \code{fars_map_state} plots the contour of the state
#' in question and points with accident locations for a given year.
#'
#' @param state.num State number, a value between 1 and 56
#' @inheritParams make_filename
#'
#'
#' @return Plot of a contour of the state in question with geographical locations
#'         of accidents (as dots) for a given year.
#'
#' If the requested year input is not present in available data, the function stops
#' and returns an error:
#' "file 'filename_YEAR.csv.bz2' does not exist".
#'
#' If the requested state number is not present in data, the function stops and
#' returns an error:
#' "invalid STATE number: number"
#'
#' @importFrom dplyr tbl_df mutate select bind_rows group_by summarize spread filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' fars_map_state("1","2013")
#' }
"fars_map_state"
#'
#' @export
fars_map_state <- function(state.num, year) {
        #This is for vignette examples
        #filename <- system.file("exdata",make_filename(year), package = "fars")
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
