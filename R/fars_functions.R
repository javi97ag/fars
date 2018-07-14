#' This is a function that reads a data file and displays it as a dataframe.

#' @param filename is a character string giving the name of the data file
#' @return This function returns the output of the data file
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @details If the file doesn't exist it will result in an error
#' @examples
#' \dontrun{fars_read("data/accident_2013.csv")}
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' This function just adds the year to the accident's data format

#' @param year an integer
#' @return This function returs a string with the filename with the year especified
#' @examples
#' \dontrun{make_filename(2013)}
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' This function reads a list of years and returns a list of data.frames.

#' @param years an integer
#' @param MONTH an integer
#' @return This function returs a list of data.frames
#' @importFrom dplyr mutate select %>%
#' @examples
#' \dontrun{fars_read_years(c(2013,2014))}
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

#' This function reads a list of years and summarizes them.


#' @param years an integer
#' @param year an integer
#' @param MONTH an integer
#' @param n an integer
#' @return This function returs a list of data.frames with the data summarized
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @examples
#' \dontrun{fars_summarize_years(c(2013,2014))}
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' create a map of fars events given a state number and a year

#' @param state.num is an integer
#' @param year an integer
#' @param STATE a character string
#' @return This functions returs a map with a point in every place there has been a fatality in a motor vehicle accident
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{fars_map_state(1,2013)}
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
