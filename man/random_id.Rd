% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random.R
\name{random_id}
\alias{random_id}
\title{Random hexadecimal identifiers}
\usage{
random_id(n = 1, bytes = 16, use_openssl = NULL, global = FALSE)
}
\arguments{
\item{n}{number of ids to return.  If \code{NULL}, it instead returns
the generating function}

\item{bytes}{The number of bytes to include for each identifier.
The length of the returned identifiers will be twice this long
with each pair of characters representing a single byte.}

\item{use_openssl}{Optionally a logical, indicating if we should
use the openssl for generating the random identifiers from the
non-global source.  If not given we prefer to use openssl if it
is available but fall back on R (See Details).}

\item{global}{Logical, indicating if random numbers should be
global (given R's global random number seed). If \code{TRUE},
then ids generated will be predictable.}
}
\description{
Random hexadecimal identifiers.  If possible, by default this uses
the \code{openssl} package to produce a random set of bytes, and
expresses that as a hex character string, creating
cryptographically secure (unpredictable) identifiers.  If that is
unavailable, fall back on the xoshiro128+ algorithm to produce
random numbers that are not cryptographically secure, but which do
not affect the global random number stream (see Details). If
desired, you can produce "predictable" random identifiers that
respect the value of the global random number stream via
\code{set.seed}.
}
\details{
Since ids version 1.2.0, the openssl package is optional, and this
affects non-global random number drawing.  If you have openssl
installed your random numbers will be ~50x faster than the
implementation we include here.

If \code{global = TRUE} we always use a simple \link{sample}
based algorithm that is driven from the global random number
stream. However, when \code{global = FALSE} the behaviour depends
on the value of \code{use_openssl} and whether that package is
installed, either using the openssl generators, using an internal
algorithm based on xoshiro128+ or erroring.
\itemize{
\item \code{use_openssl = NULL} and openssl installed: openssl
\item \code{use_openssl = NULL} and openssl missing: internal
\item \code{use_openssl = TRUE} and openssl installed: openssl
\item \code{use_openssl = TRUE} and openssl missing: error
\item \code{use_openssl = FALSE}: internal
}
}
\examples{
# Generate a random id:
ids::random_id()

# Generate 10 of them!
ids::random_id(10)

# Different length ids
random_id(bytes = 8)
# (note that the number of characters is twice the number of bytes)

# The ids are not affected by R's RNG state:
set.seed(1)
(id1 <- ids::random_id())
set.seed(1)
(id2 <- ids::random_id())
# The generated identifiers are different, despite the seed being the same:
id1 == id2

# If you need these identifiers to be reproducible, pass use_openssl = FALSE
set.seed(1)
(id1 <- ids::random_id(use_openssl = FALSE))
set.seed(1)
(id2 <- ids::random_id(use_openssl = FALSE))
# This time they are the same:
id1 == id2

# Pass `n = NULL` to generate a function that binds your arguments:
id8 <- ids::random_id(NULL, bytes = 8)
id8(10)
}
\author{
Rich FitzJohn
}
