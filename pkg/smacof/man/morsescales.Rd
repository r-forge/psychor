\name{morsescales}
\alias{morsescales}
\docType{data}
\title{Morse Code Confusion Scales}
\description{Two properties of Morse code signals. Each Morse code signal is a
sequence of up to five 'beeps'. The beeps can be short (0.05 sec) or
long (0.15 sec), and, when there are two or more beeps in a signal,
they are separated by periods of silence (0.05 sec). The two external
variables are: Signal type (1 = all short beeps, 2 = more short than long beeps, 
3 = same short and long beeps, 4 = more long than short beeps, 5 = all long beeps)
and Signal length (in seconds; 1 = .05, 2 = .15, 3 = .25, 4 = .35, 5 = .45,
6 = .55, 7 = .65, 8 = .85, 9 = .95).
}

\usage{
data(morsescales)
}

\format{Matrix of 36 morse codes by 2 properties. The first column contains the morse 
code letters.}

\references{
Rothkopf, E. Z. (1957). A measure of stimulus similarity and errors in some 
paired-associate learning. Journal of Experimental Psychology, 53, 94-101.
}

\examples{
morsescales
}
\keyword{datasets}
