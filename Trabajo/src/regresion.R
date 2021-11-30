library(tidyverse)
library(readr)

# leemos los datos
baseball <- read.csv("data/baseball/baseball.dat", comment.char = "@", header = FALSE)

# le asignamos los nombres de las columnas
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", 
					 "Doubles", "Triples", "HomeRuns", "Runs_batted_in", "Walks",
					 "Strike-Outs", "Stolen_bases", "Errors", 
					 "Free_agency_eligibility", "Free_agent", 
					 "Arbitration_eligibility", "Arbitration", "Salary")



formulas_mas_correlados <- list(Salary ~ Runs, 
								Salary ~ Hits,
								Salary ~ Doubles,
								Salary ~ HomeRuns,
								Salary ~ Runs_batted_in)

fit_lm_baseball <- lapply(formulas_mas_correlados, function(formula_lm) {
	lm(formula_lm, data = baseball)
})

lapply(fit_lm_baseball, summary)
