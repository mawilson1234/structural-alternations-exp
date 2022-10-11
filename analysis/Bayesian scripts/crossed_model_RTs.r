library(brms)
library(dplyr)

models.dir <- 'Models/Bayesian'
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

results <- read.csv('../accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	) |>
	filter(data_source == 'human') |>
	select(-data_source, -data_source.n) |>
	droplevels()

priors_crossed_RT <- c(
	set_prior('normal(0, 10)', class='Intercept'),
	set_prior('lkj(2)', class='cor'),
	set_prior('normal(0, 1)', class = 'b', coef=unlist(
		sapply(
			c(1,2,3),
			\(i) combn(
				c(
					'voice.n', 
					'target_response.n', 
					'seen_in_training.n'
				),
				m=i,
				FUN=\(x) paste(x, collapse=':')
			)
		)
	))
)

brm.args <- list(
	iter=6500, chains=4, cores=4,
	backend='cmdstanr', threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425, refresh=1
)

do.call(brm, append(brm.args, list(
	formula = RT ~ voice.n * target_response.n * seen_in_training.n +
		(1 + voice.n * target_response.n * seen_in_training.n | subject) +
		(1 | voice.n:target_response.n:seen_in_training.n:item),
	data = results,
	family = lognormal(),
	prior = priors_crossed_RT,
	file = file.path(models.dir, 'crossed_model_RTs.rds')
)))