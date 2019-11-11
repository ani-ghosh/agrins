\name{agrin_data}

\Rdversion{1.1}
 
\alias{rice_data}
\alias{ibli_data}


\title{
Get data
}

\description{
Get data used in the examples
}

\usage{
rice_data(name)
ibli_data(name)
}

\arguments{
 \item{name}{data set name}
}

\value{
A data set. Often a Spat* object or a data.frame
}



\examples{
marsabit <- agrin_data("marsabit")
loss <- ibli_data("S6C_Livestock_Losses.rds")
}



