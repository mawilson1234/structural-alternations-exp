source('Bayesian scripts/summary_functions.r')

models.dir <- file.path('Models', 'Bayesian', 'RTs')
plots.dir <- file.path('Plots', 'Bayesian', 'RTs')
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)

MAX_RT_IN_SECONDS <- 10
OUTLIER_RT_SDS <- 2

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	) |>
	filter(
		data_source == 'human',
		log.RT < log(MAX_RT_IN_SECONDS*1000)
	) |>
	group_by(subject) |>
	mutate(sd.log.RT = sd(log.RT)) |>
	filter(
		log.RT < mean(log.RT) + (OUTLIER_RT_SDS * sd.log.RT), 
		log.RT > mean(log.RT) - (OUTLIER_RT_SDS * sd.log.RT)
	) |>
	ungroup() |>
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
					'seen_in_training.n',
					'linear.n'
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

dir.create(file.path(models.dir, 'crossed'), showWarnings=FALSE, recursive=TRUE)
models <- list()
cat('Fitting crossed model (RTs)', '\n')
models['Crossed model (RTs)'] <- do.call(brm, append(brm.args, list(
	formula = RT ~ voice.n * target_response.n * seen_in_training.n * linear.n +
		(1 + voice.n * target_response.n * seen_in_training.n | linear.n:subject) +
		(1 + linear.n | voice.n:target_response.n:seen_in_training.n:item),
	data = results,
	family = lognormal(),
	prior = priors_crossed_RT,
	file = file.path(models.dir, 'crossed', 'crossed_model_RTs.rds')
))) |> list()

save_model_summaries(
	models,
	filename=file.path(models.dir, 'crossed', 'crossed_model_RTs_summary.txt'),
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename=file.path(models.dir, 'crossed', 'crossed_model_RTs_pmcmcs.txt')
)

dir.create(file.path(plots.dir, 'crossed'), showWarnings=FALSE, recursive=TRUE)
save_model_plots(
	models,
	plots.dir=file.path(plots.dir, 'crossed')
)

nesting.cols <- colnames(results)[grepl('\\.n$', colnames(results))]
nesting.cols <- nesting.cols[!grepl('data_source\\.n', nesting.cols)]
gcols <- gsub('\\.n$', '', nesting.cols)

results.with.nestings <- get_nested_data(
	data=results, 
	cols=nesting.cols,
	gcols=gcols
)

all.nested.effects <- colnames(results.with.nestings)[grepl(paste(nesting.cols, collapse='|'), colnames(results.with.nestings))]

depvar <- 'correct'
ranefs <- c('subject', 'item')
ranef.nestings <- list(
	subject='linear.n', 
	item=c('voice.n', 'target_response.n', 'seen_in_training.n')
)
nested.model.formulae <- get.nested.model.formulae(all.nested.effects, depvar, ranefs, ranef.nestings)

for (name in names(nested.model.formulae)) {
	formula <- nested.model.formulae[[name]]
	effects <- attr(terms(formula), 'term.labels')
	fixef <- effects[!grepl('^1|0 + ', effects)]
	
	nested.model.dir <- file.path(models.dir, paste0('nested_', name))
	nested.plots.dir <- file.path(plots.dir, paste0('nested_', name))
	dir.create(nested.model.dir, showWarnings=FALSE, recursive=TRUE)
	dir.create(nested.plots.dir, showWarnings=FALSE, recursive=TRUE)
	
	priors <- c(
		set_prior('normal(0, 10)', class='Intercept'),
		set_prior('lkj(2)', class='cor'),
		set_prior('normal(0, 1)', class = 'b', coef=fixef)
	)
	
	models <- list()
	cat(sprintf('Fitting nested model (RTs) %s', name), '\n')
	models[sprintf('Nested model (RTs) %s', name)] <- do.call(brm, append(brm.args, list(
		formula = formula,
		data = results.with.nestings,
		family = bernoulli(),
		prior = priors,
		file = file.path(nested.model.dir, sprintf('nested_model_RTs_%s.rds', name))
	))) |> list()
	
	save_model_summaries(
		models,
		filename=file.path(nested.model.dir, sprintf('nested_model_RTs_%s_summaries.txt', name)),
		overwrite=TRUE
	)
	
	save_pmcmc(
		models,
		filename=file.path(nested.model.dir, sprintf('nested_model_RTs_%s_pmcmcs.txt', name))
	)
	
	save_model_plots(
		models,
		plots.dir=nested.plots.dir
	)
}
