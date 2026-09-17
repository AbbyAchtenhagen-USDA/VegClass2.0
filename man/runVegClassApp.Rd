% Generated manually for packaged app launcher
\name{runVegClassApp}
\alias{runVegClassApp}
\title{Run the vegClass Shiny App}
\usage{
runVegClassApp(launch.browser = TRUE, ..., launch = TRUE)
}
\arguments{
  \item{launch.browser}{Logical. Passed to \code{shiny::runApp()}.}

  \item{...}{Additional arguments passed to \code{shiny::runApp()} when \code{launch = TRUE}.}

  \item{launch}{Logical. If \code{TRUE}, run the app. If \code{FALSE}, return the app object.}
}
\value{
A Shiny app object when \code{launch = FALSE}; otherwise starts the app.
}
\description{
Launches the packaged vegClass Shiny application.
}
\examples{
\dontrun{
runVegClassApp()
app <- runVegClassApp(launch = FALSE)
}
}
