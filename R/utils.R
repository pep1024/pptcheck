#' Returns operating system
#'
#' @return string with OS
#' @export
#'
#' @examples
#'
#' check_os()
check_os <- function(){
  if (is_windows()) {
    return('windows')
  } else {
    sysname <- Sys.info()[['sysname']]
    if (sysname == 'Linux'){
      return('linux')
    } else if (sysname == 'SunOS'){
      return('solaris')
    } else if (sysname == 'Darwin'){
      return('macOS')
    } else {
      return('non-identified')
    }
  }
}

is_windows <- function(){
  .Platform$OS.type == 'windows'
}

is_unix <- function(){
  .Platform$OS.type == 'unix'
}

is_mac <- function(){
  .Platform$OS.type == 'unix' && Sys.info()[['sysname']] == 'Darwin'
}

is_solaris <- function(){
  .Platform$OS.type == 'unix' && Sys.info()[['sysname']] == 'SunOS'
}

is_linux <- function(){
  .Platform$OS.type == 'unix' && Sys.info()[['sysname']] == 'Linux'
}

check_is_zip_file <- function(file) {
  stopifnot(file.exists(file))

  files_in <- tryCatch(utils::unzip(file, list = TRUE),
    error = function(c) {
      msg <- conditionMessage(c)
      message(c$message)
      data.frame(Name = character(0), Length = integer(0), Date=as.POSIXct(character(0)))
    })
    return(nrow(files_in) > 0)
}

open_ppt <- function(file, path = tempdir()) {
  stopifnot(file.exists(file), check_is_zip_file(file), dir.exists(path))

  utils::unzip(file, exdir = path)
  return(path)
}
