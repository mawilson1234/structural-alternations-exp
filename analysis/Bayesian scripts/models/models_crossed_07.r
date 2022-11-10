source('Bayesian scripts/summary_functions.r')

models.dir <- file.path('Models', 'Bayesian', 'accuracy_all')
plots.dir <- file.path('Plots', 'Bayesian', 'accuracy_all')
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	)

priors_crossed <- c(
	set_prior('normal(0, 10)', class='Intercept'),
	set_prior('lkj(2)', class='cor'),
	set_prior('normal(0, 1)', class = 'b', coef=unlist(
		sapply(
			c(1,2,3,4),
			\(i) combn(
				c(
					'voice.n', 
					'data_source.n', 
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
	iter=6500, 
	chains=4, 
	cores=4,
	backend='cmdstanr', 
	threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425
)

model.lists <- readRDS(file.path('Bayesian scripts', 'model_lists_accuracy_all.rds'))
dir.create(file.path(models.dir, 'crossed'), showWarnings=FALSE, recursive=TRUE)
models <- list()
i <- 7

models[sprintf('Crossed model %02d', i)] <- do.call(brm, append(brm.args, list(
	formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
	data = results |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
	family = bernoulli(),
	prior = priors_crossed,
	file = file.path(models.dir, 'crossed', sprintf('crossed_model_accuracy_%02d.rds', i))
))) |> list()

save_model_summaries(
	models,
	filename=file.path(models.dir, 'crossed', sprintf('crossed_model_accuracy_%02d_summary.txt', i)),
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename=file.path(models.dir, 'crossed', sprintf('crossed_model_accuracy_%02d_pmcmcs.txt', i))
)

dir.create(file.path(plots.dir, 'crossed'), showWarnings=FALSE, recursive=TRUE)
save_model_plots(
	models,
	plots.dir=file.path(plots.dir, 'crossed')
)