% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contemParametros.R
\name{contemParametros}
\alias{contemParametros}
\title{which parameters are missing?}
\usage{
contemParametros(funcoes, parametro, addParametro = c(), addArgs = c(),
  exclui3pontos = T)
}
\arguments{
\item{funcoes}{is a or set of functions whose param will be verify}

\item{parametro}{is list whose labels is name of param in funcoes, list of args to funcoes ex list(a="1", b="2")}

\item{addParametro}{list of param included}

\item{addArgs}{more param required}

\item{exclui3pontos}{verify por ... ? in f<-function(a, ...){ }}
}
\value{
will be returned the parameters that have not been reported in parametro and addParametro
}
\description{
this function checks whether the labels of the parameters list to move to the functions is sufficient
}
