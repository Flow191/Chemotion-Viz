FROM rocker/verse:latest

RUN install2.r --error \
	readr \
	magrittr \
	dplyr \
	sunburstR \
	Rcpp \
	ggplot2 \
	plotly
