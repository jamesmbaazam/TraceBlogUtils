% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TraceBlogTools.R
\name{create_post_folder}
\alias{create_post_folder}
\title{Create a new folder for the post}
\usage{
create_post_folder(post_folder_name, posts_dir = file.path("./posts"))
}
\arguments{
\item{post_folder_name}{A character string with the name of the folder to
store the new post. Note that the post_folder_name will be used as the
branch name so it must be valid R object name (A hyphenated name is
preferred, for example, "benchmarking-dt" but any valid R name will work).}

\item{posts_dir}{A character string with the path to the posts directory.
This is the directory where the new post folder will be created.}
}
\value{
A new folder for the post
}
\description{
Create a new folder for the post
}
\examples{
\dontrun{
create_post_folder("benchmarking-dt")
}
}
