% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proquint.R
\name{proquint}
\alias{proquint}
\title{Generate random proquint identifiers}
\usage{
proquint(
  n = 1,
  n_words = 2L,
  use_cache = TRUE,
  global = TRUE,
  use_openssl = NULL
)
}
\arguments{
\item{n}{number of ids to return.  If \code{NULL}, it instead returns
the generating function}

\item{n_words}{The number of words for each identifier; each word
has \code{2^16} (65536) possible combinations, a two-word proquint
has \code{2^32} possible combinations and an \code{k}-word proquint has
\code{2^(k * 16)} possible combinations.}

\item{use_cache}{Because there are relatively few combinations per
word, and because constructing short strings is relatively
expensive in R, it may be useful to cache all 65536 possible
words.  If \code{TRUE} then the first time that this function is
used all words will be cached and the results used - the first
time may take up to ~1/4 of a second and subsequent calls will
be much faster.  The identifiers selected will not change with
this option (i.e., given a particular random seed, changing this
option will not affect the identifiers randomly selected).}

\item{global}{Use global random number generator that responds to
\code{set.seed} (see \link{random_id} for details, but
note that the default here is different).}

\item{use_openssl}{Use openssl for random number generation when
using a non-global generator (see \link{random_id} for
details)}
}
\description{
Generate random "proquint" identifiers.  "proquint" stands for
PRO-nouncable QUINT-uplets and were described by Daniel Wilkerson
in \url{https://arxiv.org/html/0901.4016}.  Each "word" takes one of
\code{2^16} possibilities.  A four word proquint has a keyspace of
\code{10^19} possibilities but takes only 23 characters.  Proquint
identifiers can be interchanged with integers (though this is
totally optional); see \link{proquint_to_int} and the other
functions documented on that page.
}
\details{
In the abstract of their paper, Wilkerson introduces proquints:

"Identifiers (IDs) are pervasive throughout our modern
life. We suggest that these IDs would be easier to manage and
remember if they were easily readable, spellable, and
pronounceable. As a solution to this problem we propose using
PRO-nouncable QUINT-uplets of alternating unambiguous consonants
and vowels: proquints."
}
\examples{
# A single, two word, proquint
ids::proquint()

# Longer identifier:
ids::proquint(n_words = 5)

# More identifiers
ids::proquint(10)
}
