

get_problem  <- function(dn) {
  stopifnot(is.integer(dn))

  pf <- utils::download.file(paste0("https://adventofcode.com/2020/day/", dn), destfile = paste0("./problems/", dn, ".txt") )

  return(invisible())
}

get_problem(as.integer(4))