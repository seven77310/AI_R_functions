
#————————————————————————————————————————————————————————————————————————————————

#' Check and Establish Connection to EDV Database
#'
#' This function checks if a connection to the EDV database already exists and is valid.
#' If no valid connection exists, it establishes a new one.
#'
#' @return Returns an ODBC connection object to the EDV database.
#' @examples
#' \dontrun{
#' edv <- check_and_connect_edv()
#' # Use the `edv` connection for your queries
#' }
check_and_connect_edv <- function() {
  
  # Check if the connection already exists and is valid
  if (!exists("edv") || is.null(edv)) {
    message("Establishing a new connection to the EDV database...")
    
    edv <- tryCatch({
      RODBC::odbcConnect(
        dsn = ifelse(Sys.info()['sysname'] == "Windows", "edv", "tdprod1"),
        uid = Sys.getenv("USERNAME"),
        pwd = askForPassword("Please enter your database password:")
      )
    }, error = function(e) {
      message("Failed to establish a connection to the EDV database: ", e$message)
      return(NULL)
    })
    
    if (!is.null(edv)) {
      message("Connection to the EDV database established successfully.")
    } else {
      stop("Failed to establish a connection to the EDV database.")
    }
  } else {
    # Check if the existing connection is still valid by running a simple query
    tryCatch({
      test_query <- sqlQuery(edv, "SELECT 1")
      if (inherits(test_query, "try-error")) {
        stop("The existing connection is no longer valid.")
      } else {
        message("Connection to the EDV database already exists and is valid.")
      }
    }, error = function(e) {
      message("The existing connection is no longer valid. Re-establishing...")
      edv <<- RODBC::odbcConnect(
        dsn = ifelse(Sys.info()['sysname'] == "Windows", "edv", "tdprod1"),
        uid = Sys.getenv("USERNAME"),
        pwd = askForPassword("Please enter your database password:")
      )
      if (inherits(edv, "try-error")) {
        stop("Failed to re-establish a connection to the EDV database.")
      } else {
        message("Re-established connection to the EDV database successfully.")
      }
    })
  }
  
  return(edv)
}