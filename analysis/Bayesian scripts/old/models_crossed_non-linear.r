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

# generate a list of n.human.subjects integers
# within the range of the model.ids to
# fit the model using the same number of
# human and computer participants
# n.human.subjects <- results |> 
# 	filter(data_source == 'human') |>
# 	pull(subject) |>
# 	unique() |> 
# 	length()

# model.ids <- results |>
# 	filter(data_source == 'BERT') |>
# 	pull(subject) |>
# 	unique()

# N_RUNS = 10

# set.seed(425)
# model.lists <- list()
# while (!(length(unique(model.lists)) == N_RUNS)) {
# 	model.lists <- replicate(
# 		N_RUNS, 
# 		sort(
# 			sample(
# 				model.ids, 
# 				size=n.human.subjects
# 			)
# 		), 
# 		simplify=FALSE
# 	)
# }

model.lists <- readRDS(file.path('Bayesian scripts', 'model_lists_accuracy_non-linear.rds'))
dir.create(file.path(models.dir, 'crossed'), showWarnings=FALSE, recursive=TRUE)
models <- list()
for (i in seq_along(model.lists)) {
	
	models[sprintf('Crossed model (non-linear) %02d', i)] <- do.call(brm, append(brm.args, list(
		formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
			(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
			(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
		data = results |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
		family = bernoulli(),
		prior = priors_crossed,
		file = file.path(models.dir, 'crossed', sprintf('crossed_model_accuracy_non-linear_%02d.rds', i))
	))) |> list()
	
	# this ensures that if we've already fit the models and are just loading them from disk
	# that the model lists will be identical for the nested models
	model.lists[[i]] <- models[[sprintf('Crossed model (non-linear) %02d', i)]]$data$subject |> unique()
}

save_model_summaries(
	models,
	filename=file.path(models.dir, 'crossed', 'crossed_model_accuracy_non-linear_summaries.txt'), 
	overwrite=TRUE
)

save_pmcmc(
	models,
	filename=file.path(models.dir, 'crossed', 'crossed_model_accuracy_non-linear_pmcmcs.txt')
)

dir.create(file.path(plots.dir, 'crossed'), showWarnings=FALSE, recursive=TRUE)
save_model_plots(
	models,
	plots.dir=file.path(plots.dir, 'crossed')
)

# nesting.cols <- colnames(results)[grepl('\\.n$', colnames(results))]
# nesting.cols <- nesting.cols[!grepl('linear\\.n', nesting.cols)]
# gcols <- gsub('\\.n$', '', nesting.cols)

# results.with.nestings <- get_nested_data(
# 	results, 
# 	cols = nesting.cols,
# 	gcols = c('seen_in_training', 'target_response', 'data_source', 'voice')
# )

# all.nested.effects <- colnames(results.with.nestings)[grepl(paste(nesting.cols, collapse='|'), colnames(results.with.nestings))]

# depvar <- 'correct'
# ranefs <- c('subject', 'item')
# ranef.nestings <- list(
# 	subject='data_source.n', 
# 	item=c('voice.n', 'target_response.n', 'seen_in_training.n')
# )
# nested.model.formulae <- get.nested.model.formulae(all.nested.effects, nesting.cols, depvar, ranefs, ranef.nestings)

# for (name in names(nested.model.formulae)) {
# 	formula <- nested.model.formulae[[name]]
# 	effects <- attr(terms(formula), 'term.labels')
# 	fixef <- effects[!grepl('^1|0 + ', effects)]
	
# 	nested.model.dir <- file.path(models.dir, paste0('nested_', name))
# 	nested.plots.dir <- file.path(plots.dir, paste0('nested_', name))
# 	dir.create(nested.model.dir, showWarnings=FALSE, recursive=TRUE)
# 	dir.create(nested.plots.dir, showWarnings=FALSE, recursive=TRUE)
	
# 	priors <- c(
# 		set_prior('normal(0, 10)', class='Intercept'),
# 		set_prior('lkj(2)', class='cor'),
# 		set_prior('normal(0, 1)', class = 'b', coef=fixef)
# 	)
	
# 	models <- list()
# 	for (i in seq_along(model.lists)) {
# 		if (file.exists(file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_%02d.rds', name, i)))) {
# 			cat(sprintf('Loading crossed model (non-linear) %s %02d', name, i), '\n')
# 		} else {
# 			cat(sprintf('Fitting crossed model (non-linear) %s %02d', name, i), '\n')
# 		}
		
# 		models[sprintf('Nested model (non-linear) %s %02d', name, i)] <- do.call(brm, append(brm.args, list(
# 			formula = formula,
# 			data = results.with.nestings |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
# 			family = bernoulli(),
# 			prior = priors,
# 			file = file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_%02d.rds', name, i))
# 		))) |> list()
# 	}
	
# 	save_model_summaries(
# 		models,
# 		filename=file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_summaries.txt', name)),
# 		overwrite=TRUE
# 	)
	
# 	save_pmcmc(
# 		models,
# 		filename=file.path(nested.model.dir, sprintf('nested_model_accuracy_non-linear_%s_pmcmcs.txt', name))
# 	)
	
# 	save_model_plots(
# 		models,
# 		plots.dir=nested.plots.dir
# 	)
# }
