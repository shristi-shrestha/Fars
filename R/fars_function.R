#' Read a csv from fars data
#'
#' This is a simple function that checks if the file exists.
#' If it does, it returns a data frame from the input csv. If it does not exist, the function with
#' stop with an error message
#'
#' @param filename A string that will include the path to the file location
#'
#' @return Returns a data.frame containing the data.
#'
#' @examples
#' \dontrun{
#' fars_read("data/accident_2015.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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


#' Create a filename for the chosen year
#'
#' This simple function creates a filename based on the given year.Input year is an integer.
#'
#' @param year A integer representing the year
#'
#' @return Returns a string with the filename for the given year
#'
#' @examples
#' \dontrun{make_filename(2015)}
#'
#' @export
make_filename <- function(year) {
        year <- NULL
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Subset month and year
#'
#' This simple function subsets month and year from the file for the chosen year.
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return Returns a list of tibbles (data frames) with rows as each accident events and
#'    columns of month and year. Returns NULL and a warning if the file does not exist.
#'
#' @examples
#' \dontrun{fars_read_years(c(2014,2015))}
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
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


#' Summarizes number of accidents per month and year
#'
#' This simple function summarises the accident events by month and year in a chosen year(s)
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return This function returns a data.frame with months as rows
#'    and chosen year as column with counts of accident events per month and respective year(s)
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2015)
#' }
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Visualize the accident summary in US map
#'
#' Accident events are shown on the US map for a chosen state and year.
#'
#' @param state.num State number
#'
#' @param year Integer of year to use
#'
#' @return This function returns a plot of the number of accidents
#'    for the specified state.
#'
#' @examples
#' \dontrun{fars_map_state(12, 2012)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
