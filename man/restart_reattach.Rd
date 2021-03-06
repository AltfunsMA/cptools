% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{restart_reattach}
\alias{restart_reattach}
\title{Re-attaches package (restarting session)}
\usage{
restart_reattach(pkg = NULL)
}
\arguments{
\item{pkg}{An R package as a string 
(this was designed to quickly fix and test my own packages)}
}
\value{
By default, it restarts R and reattaches \code{cptools}.
Otherwise, it detaches the named package if it is loaded 
and tries to attach it again.
}
\description{
Re-attaches package (restarting session)
}
\examples{
# Another mistake in my own package
# (fixing it)
reattach_package(cptools)
}
