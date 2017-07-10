#' Read in FARS data.
#'
#' This function reads in a .csv file containing US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System ("FARS") data and returns a
#' tibble (or error message if file does not exist).
#'
#' @param filename A character string giving the name of the .csv file with FARS data
#'
#' @return This function returns a tibble with the FARS data
#'
#' @importFrom readr read_csv
#' @importFrom diplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_2014.csv")}
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

#' Make file name for FARS data file given year.
#'
#' This function converts a year to a string of the form "accident year_[YEAR].csv.bz2"
#' which corresponds to the naming convention for FARS files.
#'
#' @param year the year for the FARS file
#'
#' @return This function returns a character string a string of the form "accident
#'    year_[YEAR].csv.bz2"
#'
#' @examples
#' \dontrun{make_filename(2013)}
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read in the month & year for each accident from a FARS data file for multiple years.
#'
#' This function converts a numeric vector to a list of tibbles containing the month and
#' year columns from the FARS data file for that year.
#'
#' @param years a vector containing the years for FARS data to be included in the output
#'
#' @return This function returns a list of tibbles containing the month and year columns
#'    from the FARS data file for that year.   If there is no FARS data file for an
#'    element of \code{years} then NULL is returned for list item corresponding to that
#'    element.
#'
#' @importFrom readr read_csv
#' @importFrom diplyr mutate select tbl_df
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2015))}
#'
#' @export

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize the number of accidents by month by year for given years of FARS data.
#'
#' This function converts a numeric vector to a table containing months as rows and
#' years as columns which provides the number of accidents from FARS data.
#'
#' @param years a vector containing the years for FARS data to be included in the output
#'
#' @return This function returns a tibble with 12 rows with a MONTH column and columns
#'    for each year in \code{years} for which there is a FARS data file for that year.
#'    If there is no file for an element of \code{years} then a warning is given for
#'    that year.
#'
#' @importFrom readr read_csv
#' @importFrom diplyr bind_rows group by summarize tbl_df
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2015))}
#'
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot the number of accidents from FARS data for a given year and state.
#'
#' This function takes input corresponding to a state number and year rows and
#' returns a map of the FARS data with dots at the longitude an lattitude for each
#' fatal accident from that year.
#'
#' @param state.num a number with the state.num code for the state to be mapped
#' @param year a number for the year for FARS data accidents to be plotted in the map.
#'
#' @return This function returns a map in the Plots tab using data from the FARS data
#'    for the \code{year} input, filtered for the STATE column = \code{year}state.num
#'    If there is no file for an element of \code{years} then a warning is given for
#'    that year.
#'
#' @importFrom readr read_csv
#' @importFrom diplyr tbl_df filter
#' @importFrom maps map
#'
#' @examples
#' \dontrun{fars_map_state(55,2013)}
#'
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
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
