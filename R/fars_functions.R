#' Function to read FARS file.
#'
#' This function uses readr and dplyr packages.
#' It takes a CSV file as an input filename parameter.
#' If the file does not exist in the working directory, this function stops with an output message that the file does not exist.
#' If the file exists, this function reads it into dataframe object.
#' It suppresses messages and progress bar while reading the file.
#'
#' @importFrom  readr read_csv
#' @importFrom dplyr tbl_df
#' @param filename A filename as input string.
#'
#' @return File data is read into a dataframe object.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read(filename="accident_2013.csv.bz2")
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

#' Function to create FARS filename with the year parameter.
#'
#' This function takes integer or string as year parameter.
#' It converts year parameter as integer.
#' It returns output filename as string with the input year.
#'
#' @param year Valid year as integer or string.
#'
#' @return Filename with the year parameter in the filename as string
#'
#' @examples
#' make_filename(2012)
#' make_filename(year=2012)
#' make_filename("2012")
#' make_filename(year="2012")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Function to reads multiple files based on years parameter.
#'
#' This function takes the years parameter.
#' It reads multiple files, one at a time, for each year and returns a dataframe object with data for all years.
#' It outputs a warning message if it cannot find a file for a year in the years parameter. It returns NULL.
#' It uses functions: make_filename and fars_read.
#'
#' @importFrom dplyr mutate select
#' @param years Valid years as a vector (atomic or list) or an expression object.
#'
#' @return File data is read into a dataframe object by MONTH and year.
#'
#' @examples
#' fars_read_years(list(2013, 2014, 2015))
#' fars_read_years(year=list(2013, 2014, 2015))
#' fars_read_years(list("2013", "2014", "2015"))
#' fars_read_years(year=list("2013", "2014", "2015"))
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

#' Function to reads multiple files based on years parameter and sumarizes data.
#'
#' This function takes the years parameter.
#' It reads multiple files and returns dataframe object.
#' It outputs a warning message if it cannot find a file for a year in the years parameter. It returns NULL.
#' It uses function: fars_read_years
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr gather
#' @param years Valid years as a vector (atomic or list) or an expression object.
#'
#' @return File data is read and sumarized into a dataframe object by MONTH and year.
#'
#' @examples
#' fars_summarize_years(list(2013, 2014, 2015))
#' fars_summarize_years(year=list(2013, 2014, 2015))
#' fars_summarize_years(list("2013", "2014", "2015"))
#' fars_summarize_years(year=list("2013", "2014", "2015"))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Function to reads a file based on year parameter and plots accident data based on state parameter.
#'
#' This function takes state and year as parameters.
#' It reads file based on year parameter and gathers data to plot based on state number parameter.
#' It converts state number parameter as integer and stops if it cannot find state number is invalid.
#' It outputs a message if does not find any accidents to plot.
#' It returns the data plotted for state number by LONGITUDE > 900 and LATITUDE > 90
#' It uses functions: make_filename and fars_read
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @param year Valid year as integer or string.
#' @param state.num Valid State number as string or integer.
#'
#' @return A plot of accidents by longitude and latitude for state number and year.
#'
#' @examples
#' fars_map_state(25, 2015)
#' fars_map_state(state.num=25, year=2015)
#' fars_map_state("25", "2015")
#' fars_map_state(state.num="25", year="2015")
#' fars_map_state(25, "2015")
#' fars_map_state(state.num=25, year="2015")
#' fars_map_state("25", 2015)
#' fars_map_state(state.num="25", year=2015)
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
