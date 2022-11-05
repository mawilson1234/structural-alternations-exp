source('Bayesian scripts/summary_functions.r')

models.dir <- file.path('Models', 'Bayesian', 'accuracy_cossims')
plots.dir <- file.path('Plots', 'Bayesian', 'accuracy_cossims')
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)

results <- read.csv('accuracy-data.csv') |>
	filter(data_source == 'BERT')

brm.args <- list(
	iter=6500, 
	chains=4, 
	cores=4,
	backend='cmdstanr', 
	threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425
)

nesting.cols <- colnames(results)[grepl('\\.n$', colnames(results))]
nesting.cols <- nesting.cols[!grepl('((data_source|linear)\\.n|mean_cossim_to_targets)', nesting.cols)]
gcols <- gsub('\\.n$', '', nesting.cols)

results.with.nestings <- get_nested_data(
	data=results, 
	cols=nesting.cols,
	gcols=gcols
)

nested.model.formulae <- readRDS(file.path('Bayesian scripts', 'nested_model_formulae_cossims.rds'))

name <- names(nested.model.formulae)[[3]]

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
cat(sprintf('Fitting nested model (cossims) %s', name), '\n')
models[sprintf('Nested model (cossims) %s', name)] <- do.call(brm, append(brm.args, list(
	formula = formula,
	data = results.with.nestings,
	family = bernoulli(),
	prior = priors,
	file = file.path(nested.model.dir, sprintf('nested_model_cossims_%s.rds', name))
))) |> list()

save_model_summaries(
	models,
	filename=file.path(nested.model.dir, sprintf('nested_model_cossims_%s_summary.txt', name)),
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename=file.path(nested.model.dir, sprintf('nested_model_cossims_%s_pmcmcs.txt', name))
)

save_model_plots(
	models,
	plots.dir=nested.plots.dir
)