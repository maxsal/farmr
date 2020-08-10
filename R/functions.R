#' Plant a seed
#'
#' This function creates a R project folder, automatically creating a
#' subdirectory system I've found useful.
#'
#' @param project_name Name of the project (this names the project folder and
#' .Rprof file)
#' @return Opens a new RStudio session with the newly created project
#' @export
plant_seed <- function(project_name = NULL) {

  if (is.null(project_name)) {
    p_name <- stringr::str_extract(getwd(), "([^/]+$)")
    base   <- stringr::str_extract(getwd(), "(.*)/")
    p_path <- glue::glue("{base}{p_name}")
  } else {
    p_name <- project_name
    base   <- glue::glue("{getwd()}/")
    p_path <- glue::glue("{base}{p_name}")
    dir.create(p_path, showWarnings = FALSE)
  }

  print(glue::glue("Planting seed in {p_path}..."))

  # initiate .Rproj ----------
  if (file.exists(glue::glue("{p_path}/{p_name}.Rproj"))) {

  } else {

    rproj_con <- file(glue::glue("{p_path}/{p_name}.Rproj"))
    writeLines(c(
      "Version: 1.0",
      "",
      "RestoreWorkspace: Default",
      "SaveWorkspace: Default",
      "AlwaysSaveHistory: Default",
      "",
      "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes",
      "NumSpacesForTab: 4",
      "Encoding: UTF-8",
      "",
      "RnwWeave: knitr",
      "LaTeX: pdfLaTeX"
    ),
    rproj_con)
    close(rproj_con)

  }

  # initiate log ----------
  readr::write_rds(
    tibble::tibble(
      entry_id   = 0,
      entry_date = Sys.Date(),
      entry_time = format(Sys.time(), "%H:%M"),
      entry      = "initialize project tree"
    ),
    path = glue::glue("{p_path}/diary.rds")
  )

  # initiate .Rprofile
  if (file.exists(glue::glue("{p_path}/.Rprofile"))) {

  } else {
    rprof_con <- file(glue::glue("{p_path}/.Rprofile"))
    writeLines(c(
      ".First <- function() {",
      "",
      "  options(usethis.protocol = 'ssh')",
      "  options(renv.consent = TRUE)",
      "",
      "  if (!('renv' %in% list.files())) {",
      "    library('here')",
      "    library('usethis')",
      "    library('tidyverse')",
      "  }",
      "  dir.create(here::here('figs'), showWarnings = F)",
      "  dir.create(here::here('bin'), showWarnings = F)",
      "  dir.create(here::here('data'), showWarnings = F)",
      "  dir.create(here::here('scripts'), showWarnings = F)",
      "  dir.create(here::here('data', 'raw'), showWarnings = F)",
      "  dir.create(here::here('data', 'processed'), showWarnings = F)",
      "  dir.create(here::here('data', 'results'), showWarnings = F)",
      "",
      "  if (!('renv' %in% list.files())) {",
      "    renv::init()",
      "    suppressMessages(renv::install(c('tidyverse', 'usethis', 'here', 'gert', 'devtools')))",
      "    devtools::install('~/Documents/projects/farmr/')",
      # "    usethis::use_git()",
      # "    usethis::use_github()",
      "  } else {",
      # "    list.of.packages <- c('renv', 'tidyverse', 'here', 'glue', 'usethis')",
      # "    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, 'Package'])]",
      # "    if(length(new.packages)) renv::install(new.packages)",
      "    suppressMessages(library('tidyverse'))",
      "    suppressMessages(library('here'))",
      "    suppressMessages(library('glue'))",
      "    suppressMessages(library('usethis'))",
      "    source('renv/activate.R')",
      "  }",
      "",
      "  cat('\nWelcome to your R-Project:', here(), '\n')",
      "}",
      ""
    ),
    rprof_con)
    close(rprof_con)

  }

  system2("open", glue::glue("{p_path}/{p_name}.Rproj"))

}

# add to diary ----------
#' @export
dear_diary <- function(entry) {

  tmp_diary <- readr::read_rds(here::here("diary.rds"))

  new_id <- max(tmp_diary$entry_id) + 1

  readr::write_rds(tmp_diary %>%
              add_row(
                "entry_id"   = new_id,
                "entry_date" = Sys.Date(),
                "entry_time" = format(Sys.time(), "%H:%M"),
                "entry"      = entry,
              ),
            here::here("diary.rds")
  )

  print(glue::glue("Entry {new_id} recorded!!!"))

}

# read diary -----------
#' @export
read_diary <- function(id = NULL, date = NULL) {

  if (!is.null(id)) {
    readr::read_rds(here::here("diary.rds")) %>%
      dplyr::filter(entry_id == id)
  } else if (!is.null(date)) {
    readr::read_rds(here::here("diary.rds")) %>%
      dplyr::filter(entry_date == date)
  } else {
    readr::read_rds(here::here("diary.rds"))
  }

}

# library card ----------
#' @export
library_card <- function() {

  list_of_packages <- c('renv', 'tidyverse', 'here', 'glue', 'devtools')
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, 'Package'])]
  if (length(new_packages)) {
    renv::install(new_packages)
  } else {
    print(glue::glue("Books already checked out"))
  }

}

# get git ----------
#' @export
get_git <- function() {

  usethis::use_git()
  usethis::use_github()

}