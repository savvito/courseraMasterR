
#' CSV File Reader.
#' @description  Reads a csv file, given the filename.
#' @Packages_required  'readr' and 'dplyr' must  be loaded first.
#' @param filename This should be a character value which consists of the name of the file that we
#' want to load. Please note that the whole path is needed in case the file is not in the working directory
#' @return The data from the csv file loaded in an object of class 'tbl_df'.
#' @examples
#' mydata <- fars_read("accident_2013.csv.bz2")
#' str(mydata)
#' ## if we try to load again the above file but forget to type the _ character it will throw an 
#' error saying file accident_2013.csv.bz2 does not exist
#' mydata <- fars_read("accident2013.csv.bz2")





fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' File Name Generator.
#' @description  Generates a filename (character) according to the format of US National Highway 
#' Traffic Safety Administration's Fatality Analysis Reporting System datasets
#' @param year An integer number which represents The year for which the filename has to be generated.
#' @return a character vector with The filename in the above mentioned format for the year which has been supplied.
#' @examples
#' myfilename <- make_filename(2013)
#' str(myfilename)



make_filename  <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' extracting 'Year' & 'Month'  data.
#' @description Subsets Year & Month variables from the data, if the file is present in the working directory
#'             (in case year = 2013, 2014, 2015), otherwise prints 'Invalid year'.
#' @Packages_required  'readr' and 'dplyr' must  be loaded first.
#' @param years An integer number which represents The year for which the file would be read and the year and month extracted
#'
#' @return The data present in the file (in case year = 2013, 2014, 2015). This is an object of class 'tbl_df'.
#' NULL is returned for each input year not corresponding to a valid dataset.
#' @examples
#' ## Considering your data is in the present working directory
#' ## The following would work fine and would return the subsetted data.
#' mydata <- fars_read_years(2013)
#' str(mydata)
#' ## The following would work fine and would return a list of length 1 with NULL object.
#' mydata <- fars_read_years(2019)

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



#' Summarize data per 'Year' and 'Month' 
#' @description Summarizes the data per year and month
#' @param years An integer number representing the year for which you want to summurize the data
#' @Packages_required  'readr' , 'dplyr'and 'tidyr' must  be loaded first.
#' @return The count of number of observations present in the file per momth. in case there is no file it returns an 
#' error 
#' @examples
#' mydata <- fars_summarize_years(2013)
#' str(mydata)
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}




#' Plots accidents locations on a map
#' @description Plots accidents occuring in a particular state on the state map .
#' @param years An integer number representing the year for which you want to plot the data
#' @param state.num An integer representing one of the states in US. it must be from 1 TO %^>
#' @Packages_required  'readr' , 'dplyr', tidyr' and maps must  be loaded first.
#' @return A map of the states with the accidents plotted on it
#'
#' @examples
#' ## The following would work fine and would return the subsetted data.
#' mydata <- fars_map_state(55, 2013)
#' str(mydata)



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
