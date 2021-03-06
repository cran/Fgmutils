% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getggplot2GraphicObservadoXEstimado.R
\name{getggplot2GraphicObservadoXEstimado}
\alias{getggplot2GraphicObservadoXEstimado}
\title{Get ggplot2 Grapic observed versus estimated}
\usage{
getggplot2GraphicObservadoXEstimado(titulo = "observadoXestimado",
  nome = "observadoXestimado", observado, estimado,
  identificadorIndividual = NULL, identificadorGrupal = NULL,
  showTestF = TRUE, TestFposition = 4,
  titleIdentificadorGrupal = NULL, save = NULL, labsX = "observado",
  labsy = "estimado", nomeParaExibir = NULL, environ = 1,
  extensao = ".png", ...)
}
\arguments{
\item{titulo}{is the title graphic}

\item{nome}{name of file case save}

\item{observado}{list containing the observations of variable}

\item{estimado}{list containing estimates of variable}

\item{identificadorIndividual}{list containing 'id' of individuals}

\item{identificadorGrupal}{list containing group of individuals}

\item{showTestF}{draw results of test F in graphic?}

\item{TestFposition}{show one of the four corners of the graph clockwise}

\item{titleIdentificadorGrupal}{title of Legend of the groups}

\item{save}{If you want to save enter the directory as a string}

\item{labsX}{label x}

\item{labsy}{label y}

\item{nomeParaExibir}{This is the name to display the graph as a function after the completion of this}

\item{environ}{environment in which the function to display the ggplot2 must be saved}

\item{extensao}{type of image that will be saved}

\item{...}{only for compatibility with other functions}
}
\value{
will be returned the graphical generated by ggplot2
}
\description{
this function displays/saves/returns a Graphical ggplot2 illustrating the difference between the observed and estimated
}
