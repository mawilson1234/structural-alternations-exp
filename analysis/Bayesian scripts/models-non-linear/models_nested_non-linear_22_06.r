source('Bayesian scripts/summary_functions.r')

models.dir <- file.path('Models', 'Bayesian', 'accuracy_non-linear_only')
plots.dir <- file.path('Plots', 'Bayesian', 'accuracy_non-linear_only')
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)

results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item 	= as.factor(item)
	) |> 
	filter(linear == 'Non-linear') |>
	droplevels()

brm.args <- list(
	iter=6500,
	chains=4, 
	cores=4,
	backend='cmdstanr', 
	threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425
)

model.lists <- readRDS(file.path('Bayesian scripts', 'model_lists_accuracy_non-linear.rds'))

nesting.cols <- colnames(results)[grepl('\\.n$', colnames(results))]
nesting.cols <- nesting.cols[!grepl('linear\\.n', nesting.cols)]
gcols <- gsub('\\.n$', '', nesting.cols)

results.with.nestings <- get_nested_data(
	results, 
	cols = nesting.cols,
	gcols = gcols
)

nested.model.formulae <- readRDS(file.path('Bayesian scripts', 'nested_model_formulae_accuracy.rds'))

name <- names(nested.model.formulae)[[22]]

nested.model.dir <- file.path(models.dir, paste0('nested_', name))
nested.plots.dir <- file.path(plots.dir, paste0('nested_', name))
dir.create(nested.model.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(nested.plots.dir, showWarnings=FALSE, recursive=TRUE)

formula <- nested.model.formulae[[name]]
effects <- attr(terms(formula), 'term.labels')
fixef <- effects[!grepl('^1|0 + ', effects)]
priors <- c(
	set_prior('normal(0, 10)', class='Intercept'),
	set_prior('lkj(2)', class='cor'),
	set_prior('normal(0, 1)', class = 'b', coef=fixef)
)

models <- list()
i <- 6

if (file.exists(file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_%02d.rds', name, i)))) {
	cat(sprintf('Loading crossed model (non-linear) %s %02d', name, i), '\n')
} else {
	cat(sprintf('Fitting crossed model (non-linear) %s %02d', name, i), '\n')
}
	
models[sprintf('Nested model (non-linear) %s %02d', name, i)] <- do.call(brm, append(brm.args, list(
	formula = formula,
	data = results.with.nestings |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
	family = bernoulli(),
	prior = priors,
	file = file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_%02d.rds', name, i))
))) |> list()

save_model_summaries(
	models,
	filename=file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_%02d_summary.txt', name, i)),
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename=file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_%02d_pmcmcs.txt', name, i))
)

save_model_plots(
	models,
	plots.dir=nested.plots.dir
)
