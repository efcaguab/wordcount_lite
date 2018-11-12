#' wordcountaddin
#'
#' This packages is an addin for RStudio that will count the words and characters in a plain text document. It is designed for use with R markdown documents and will exclude YAML header content, code chunks and inline code from the counts. It also computes readability statistics so you can get an idea of how easy or difficult your text is to read.
#'
#' @name wordcountaddin_lite
#' @docType package
#' @import stringi
NULL

#-------------------------------------------------------------------
# fns for working with selected text in an active Rmd

#' Get text stats for selected text (excluding code chunks and inline code)
#'
#' Call this addin to get a word count and some other stats about the text
#'
#' @export
text_stats <- function(filename = "") {

  md_file_ext_regex <- paste(
    "\\.markdown$",
    "\\.mdown$",
    "\\.mkdn$",
    "\\.md$",
    "\\.mkd$",
    "\\.mdwn$",
    "\\.mdtxt$",
    "\\.mdtext$",
    "\\.rmd$",
    "\\.Rmd$",
    "\\.RMD$",
  sep = "|")

  text_to_count <-

  if (nchar(filename) > 0) {
    # if a filename is supplied, check that it is a md or rmd file
    if (!grepl(md_file_ext_regex, filename)) {
           stop(paste("The supplied file has a file extension which is not associated with markdown.",
                      "This function only works with markdown or R markdown files.", sep = "\n  "))
    } else {
      # if we have an md or Rmd file, read it in as a character vector
      paste(scan(filename, 'character', quiet = TRUE), collapse = " ")
    }

    } else  {
      stop()
  }
  text_stats_fn(text_to_count)
}


#---------------------------------------------------------------
# directly work on a character string in the console


#' Get text stats for selected text (excluding code chunks and inline code)
#'
#' Use this function with a character string as input
#'
#' @export
text_stats_chr <- function(text) {

  text <- paste(text, collapse = "\n")

  text_stats_fn(text)

}


#-----------------------------------------------------------
# helper fns

prep_text <- function(text){

  # remove all line breaks, http://stackoverflow.com/a/21781150/1036500
  text <- gsub("[\r\n]", " ", text)

  # don't include front yaml
  text <- gsub("---.*--- ", "", text)

  # don't include text in code chunks: https://regex101.com/#python
  text <- gsub("```\\{.+?\\}.+?```", "", text)

  # don't include text in in-line R code
  text <- gsub("`r.+?`", "", text)

  # don't include HTML comments
  text <- gsub("<!--.+?-->", "", text)

  # don't include LaTeX comments
  # how to do this? %%

  # don't include inline markdown URLs
  text <- gsub("\\(http.+?\\)", "", text)

  # don't include images with captions
  text <- gsub("!\\[.+?\\)", "", text)

  # don't include # for headings
  text <- gsub("#*", "", text)

  # don't include html tags
  text <- gsub("<.+?>|</.+?>", "", text)

    # don't include percent signs because they trip up stringi
  text <- gsub("%", "", text)

  # don't include LaTeX \eggs{ham}
  # how to do? problem with capturing \x


  if (nchar(text) == 0) {
    stop("You have not selected any text. Please select some text with the mouse and try again")
  } else {

  return(text)

  }

}


# These functions do the actual work
text_stats_fn_ <- function(text){
  # suppress warnings
  oldw <- getOption("warn")
  options(warn = -1)

  text <- prep_text(text)

  # stringi methods
  n_char_tot <- sum(stringi::stri_stats_latex(text)[c(1,3)])
  n_words_stri <- unname(stringi::stri_stats_latex(text)[4])

   # reading time
  # https://en.wikipedia.org/wiki/Words_per_minute#Reading_and_comprehension
  # assume 200 words per min
  wpm <-  200
  reading_time_stri <- paste0(round(n_words_stri / wpm, 1), " minutes")

  return(list(
  # make the names more useful
  n_char_tot_stri = n_char_tot,
  n_words_stri = n_words_stri,
  reading_time_stri = reading_time_stri
  ))

  # resume warnings
  options(warn = oldw)

}


text_stats_fn <- function(text){
  text_stats_fn_(text)
}
