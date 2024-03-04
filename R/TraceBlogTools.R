#' Create a new folder for the post
#'
#' @param post_folder_name A character string with the name of the folder to
#' store the new post. Note that the post_folder_name will be used as the
#' branch name so it must be valid R object name (A hyphenated name is
#' preferred, for example, "benchmarking-dt" but any valid R name will work).
#' @param posts_dir A character string with the path to the posts directory.
#' This is the directory where the new post folder will be created.
#' @importFrom checkmate assert_character
#' @importFrom cli cli_alert_success
#' @return A new folder for the post
#' @export
#'
#' @examples
#' \dontrun{
#' create_post_folder("benchmarking-dt")
#' }
create_post_folder <- function(post_folder_name,
                               posts_dir = file.path("./posts")) {
  checkmate::assert_character(post_folder_name)
  # Define the posts directory
  new_post_dir <- file.path(posts_dir, post_folder_name)
  cli::cli_rule(
    "Creating {.file {new_post_dir}}"
  )
  dir.create(new_post_dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert_success(
    "The folder {.file {new_post_dir}} has been created."
  )
}

#' Create a new git branch
#'
#' @inheritParams create_post_folder
#' @importFrom checkmate assert_character
#' @importFrom cli cli_rule cli_abort cli_alert_info
#' @importFrom git2r branch_create checkout pull status
#' @return A new git branch
#' @export
#' @examples
#' \dontrun{
#' create_post_git_branch("benchmarking-dt")
#' }
create_post_git_branch <- function(post_folder_name) {
  checkmate::assert_character(post_folder_name)
  # Need to pull the latest changes from the main branch
  if (!is.null(git2r::status())) {
    #nolint start: keyword_quote_linter
    cli::cli_abort(
      c(
        "!" = "You have uncommitted changes.",
        "i" = "Please commit your changes before creating a new branch."
      )
    )
    #nolint end
  }
  # We prefer to create a new branch from the main branch
  cli::cli_alert_info(
    "Checking out and pulling the latest changes from the main branch"
  )
  git2r::checkout("main")
  git2r::pull()
  # The new branch name must nto already exist
  if (post_folder_name %in% names(git2r::branches())) {
    #nolint start: keyword_quote_linter
    cli::cli_abort(
      c(
        "!" = "Branch already exists.",
        "i" = "Please choose a different branch name."
      )
    )
    #nolint end
  }
  cli::cli_rule(
    "Creating branch {.emph {post_folder_name}}."
  )
  git2r::branch_create(post_folder_name)
  git2r::checkout(post_folder_name)
  #nolint start: object_usage_linter
  cli::cli_rule(
    "You are now on branch {.emph {post_folder_name}}."
  )
  #nolint end
}

#' Automate steps for contributing blogposts to the Epiverse-Trace blog.
#'
#' @description
#' This is the main function that automates the steps outlined in the
#' [contributing guide](https://github.com/epiverse-trace/epiverse-trace.github.io/blob/main/.github/CONTRIBUTING.md#step-by-step)). #nolint
#' It creates a git branch using the name of the post folder, adds the
#' directory "./posts/<post_folder_name>/ folder, and adds a boiler plate
#' `index.qmd` file.
#'
#' @inheritParams create_post_folder
#' @param posts_dir A character string with the path to the posts directory
#' @param title A character string with the title of the post
#' @param subtitle A character string with the subtitle of the post
#' @param author A character string with the author of the post
#' @param open_file A logical value indicating whether to open the index.qmd
#' file after it is created
#' @importFrom checkmate assert_character
#' @importFrom cli cli_alert_success cli_alert_info
#' @importFrom rstudioapi documentOpen
#'
#' @return A new index.qmd file
#' @export
#'
#' @examples
#' \dontrun{
#' create_index_qmd_file("benchmarking-dt")
#' }
create_index_qmd_file <- function(post_folder_name,
                                  posts_dir = file.path("./posts"),
                                  title = NULL,
                                  subtitle = NULL,
                                  author = NULL,
                                  open_file = FALSE) {
  checkmate::assert_character(post_folder_name)
  checkmate::assert_character(posts_dir)
  checkmate::assert_character(title, null.ok = TRUE)
  checkmate::assert_character(subtitle, null.ok = TRUE)
  checkmate::assert_character(author, null.ok = TRUE)

  # Create a new git branch
  create_post_git_branch(post_folder_name)

  # Create a new folder for the post
  create_post_folder(post_folder_name, posts_dir)

  # Create the boiler plate
  index_qmd_path <- file.path(
    posts_dir,
    post_folder_name,
    "index.qmd"
  )

  boilerplate_content <- c(
    "---",
    paste("title:", ifelse(!is.null(title), title, "Your Title Here")),
    if (!is.null(subtitle)) paste("subtitle:", subtitle),
    "date: '`r format(Sys.Date(), '%Y-%m-%d')`'",
    paste("author:", ifelse(!is.null(author), author, "Your Name")),
    "---",
    "",
    "# h1",
    "",
    "Some text.",
    "",
    "# h1",
    "",
    "Some more text."
  )

  writeLines(boilerplate_content, index_qmd_path)

  # Print a message to the user
  #nolint start: keyword_quote_linter object_usage_linter
  cli::cli_alert_success(
    c(
      "i" = "A boilerplate {.val index.qmd} has been added to the path
      {.file {index_qmd_path}}."
    )
  )
  #nolint end
  cli::cli_alert_info(
    "Don't forget to stage, commit, and push your changes."
  )
  if (open_file) {
    rstudioapi::documentOpen(index_qmd_path)
  }
}
