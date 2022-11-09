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

nesting.cols <- colnames(results)[grepl('\\.n$', colnames(results))]
nesting.cols <- nesting.cols[!grepl('data_source\\.n', nesting.cols)]
gcols <- gsub('\\.n$', '', nesting.cols)

results.with.nestings <- get_nested_data(
	data=results, 
	cols=nesting.cols,
	gcols=gcols
)

nested.model.formulae <- readRDS(file.path('Bayesian scripts', 'nested_model_formulae_RTs.rds'))

name <- names(nested.model.formulae)[[13]]

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
	family = lognormal(),
	prior = priors,
	file = file.path(nested.model.dir, sprintf('nested_model_RTs_%s.rds', name))
))) |> list()
	
save_model_summaries(
	models,
	filename=file.path(nested.model.dir, sprintf('nested_model_RTs_%s_summary.txt', name)),
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