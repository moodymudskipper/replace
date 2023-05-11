#' Replace occurence of variable in files
#'
#' Use to replace a variable name in a collection of files, keeping control on the type of
#' replacement.
#'
#' @param paths a vector of paths or folders, if the latter all R scripts (".R", ".r") are considered
#' @param target a string
#' @param replacement a string
#' @param what one or several of "var", "fun", "arg", "formal", "package". They are
#'   mapped to token types found in the output of `getParseData()`. See details.
#'
#' * "var" : standard variable names `target`
#' * "fun" : function calls `target()` (if called through lapply etc a function will be matched with "var", not "fun")
#' * "arg" : Function argument name `foo(target = bar)`
#' * "formal" : a function's formal definition `function(target) {}` or `function(target = ) {}`
#' * "package" : `target::foo` or `target:::foo`
#'
#' @return
#' @export
#'
#' @examples
replace_in_files <- function(target, replacement, paths = "R", what = c("var", "fun")) {
  what <- match.arg(what, several.ok = TRUE, choices =  c("var", "fun", "arg", "formal", "package"))
  what <- c(
    var = "SYMBOL",
    fun = "SYMBOL_FUNCTION_CALL",
    arg = "SYMBOL_SUB",
    formal = "SYMBOL_FORMALS",
    package = "SYMBOL_PACKAGE"
  )[what]
  paths <- unlist(lapply(paths, function(path) {
    if (dir.exists(path)) return(list.files(path = path, pattern = "\\.[rR]$", recursive = TRUE, full.names = TRUE))
    path
  }))
  for (path in paths) {
    lines <- readLines(path)
    pd <- subset(
      getParseData(parse(file = path), includeText = TRUE),
      token %in% what & text == target
    )
    # start from the end and loop in case there are several replacement per line
    for (i in rev(seq_len(nrow(pd)))) {
      row <- pd[i,]
      stringr::str_sub(lines[[row$line1]], row$col1, row$col2) <- replacement
    }
    writeLines(lines, path)
  }
  invisible(NULL)
}
