# Load libraries
library(brms)
library(tools)
library(tidyr)
library(dplyr)
library(ggpubr)
library(R.utils)
library(bayesplot)

# Create directories to store results
base.dirname <- 'C:/Users/mawilson/OneDrive - Yale University/CLAY Lab/structural-alternations-exp/analysis'
plots.dir <- file.path(base.dirname, 'Plots/Bayesian')
models.dir <- file.path(base.dirname, 'Models/Bayesian')
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

bayesplot_theme_set(theme_default(base_family = getOption('bayesplot.base_family', 'sans')))

posteriors_plot <- function(x, pars = '', labels = '', title = '', color_scheme = ''){
	if (color_scheme == ''){
		color_scheme_set('gray')
	} else {
		color_scheme_set(color_scheme)
	}
	
	if (title == ''){
		title <- deparse(substitute(x))
		title <- gsub('\\.', ' ', title)
		title <- paste(title, 'model posteriors')
	}
	
	if (pars == ''){
		pars <- rev(colnames(x)[grepl('^b\\_(?!Intercept)', colnames(x), perl = TRUE)])
	}
	
	if (labels == '') {
		labels <- gsub('^b\\_', '', pars)
		labels <- gsub('\\.n(:|$)', '\\1', labels)
		labels <- gsub('(\\.|\\_)', ' ', labels)
		labels <- gsub(':', ' × ', labels)
		labels <- toTitleCase(labels)
	} else if (length(labels) != length(pars)) {
		cat("Warning: the number of labels doesn't match the number of parameters to plot!")
		cat('Some parameters may not be labeled, or may be labeled incorrectly.')
	}
	
	# for some reason the pars are plotted in reverse, so we need to reverse
	# the labels to make it line up (???)
	plot <- mcmc_areas(x, pars=pars, prob=0.95, prob_outer=0.99, point_est='mean') +
		expand_limits(x=0) +
		scale_x_continuous('', n.breaks=8) +
		scale_y_discrete(labels=labels) +
		ggtitle(title)
	
	x_range <- range(plot$data$x)
	
	plot <- plot +
		theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), 'cm'))
	
	color_scheme_set()
	
	return (plot)
}

save_model_summaries <- function(
	models = list(), 
	filename = '', 
	overwrite = FALSE
){
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''), '\n')
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	if (filename == '') {
		filename <- file.path(models.dir, 'model_summaries.txt')
	} else {
		filename <- file.path(models.dir, filename)
	}
	
	if (file.exists(filename) & !overwrite){
		cat(paste0('File "', filename, '" already exists. Use overwrite = TRUE to overwrite.\n'))
		return ()
	}
	
	text <- ''
	
	withOptions(
		{
			for (model_name in names(models)){
				cat('Processing model "', model_name, '"...\n', sep = '')
				text <- paste0(text, topsep, model_name, midsep)
				
				output <- capture.output(print(summary(models[[model_name]])))
				for(line in output){
					if (grepl('^Formula:|\\$(.*):', line)) {
						line <- gsub('\\s+', ' ', line)
						pad <- ifelse(grepl('^Formula:', line), paste0(rep(' ', nchar('Formula: ')), collapse = ''), '\t')
						line <- gsub('(\\+ \\([01](.*?)\\|(\\|)?(.*?)\\))', paste0('\n', pad, '\\1'), line)
					}
					text <- paste0(text, line, '\n')
				}
				text <- paste0(text, botsep)
			}
			
			sink(filename)
			cat(text)
			sink()
		},
		# Increase the maximum printing range for saving results to files to work correctly
		max.print = 100000,
		width = 10000
	)
}

save_pmcmc <- function(
	models = list(),
	filename = '',
	variable = '^b\\_',
	regex = TRUE,
	max_digits = 4
) {
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''))
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	text <- ''
	
	printformat <- paste0('%.0', max_digits, 'f')
	
	if (filename == '') {
		filename <- file.path(models.dir, 'model_pmcmcs.txt')
	} else {
		filename <- file.path(models.dir, filename)
	}
	
	withOptions(
		{
			for (model_name in names(models)){
				model <- models[[model_name]]
				
				posteriors <- as_draws_df(model, variable=variable, regex=regex)
				
				summary <- posteriors |>
					select(-`.chain`, -`.iteration`, -`.draw`) |>
					pivot_longer(everything()) |>
					group_by(name) |>
					summarize_all(list(sum=\(x) sum(x > 0),length=length)) |>
					mutate(p_mcmc = sum/length) |>
					select(name, p_mcmc)
				
				text <- paste0(text, topsep, model_name, ' posteriors', midsep)
				
				for (i in seq_len(nrow(summary))) {
					effect <- gsub('^b\\_', '', summary[i,'name'][[1]])
					effect <- gsub('\\.n(:|$)', '\\1', effect)
					effect <- gsub(':', ' x ', effect)
					effect <- gsub('(\\.|\\_)', ' ', effect)
					effect <- toTitleCase(effect)
					pmcmc <- summary[i, 'p_mcmc'][[1]]
					dir <- ifelse(pmcmc > 0.5, ' < 0', ' > 0')
					pmcmc <- ifelse(pmcmc > 0.5, 1 - pmcmc, pmcmc)
					text <- paste0(text, '\n', sprintf(printformat, pmcmc), ': ', effect, dir)
				}
				text <- paste0(text, botsep)
			}
			
			text <- gsub('\\n$', '', text)
			
			sink(filename)
			cat(text)
			sink()
		},
		max.print = 100000,
		width = 10000
	)
}

save_model_plots <- function(models = list()) {	
	for (model_name in names(models)) {	
		# we have to put every plot in a list or else R flattens them out
		# and they're unusable. amazing behavior. great language
		model <- models[[model_name]]
		plots <- list()
		plot_types <- list('trace plot'='trace', 'marginal posteriors'='hist')
		variables <- list('(slopes)'='^b\\_', '(standard deviations)'='^sd\\_', '(correlations)'='^cor\\_')
		for (plot_type in names(plot_types)){
			for (variable in names(variables)){
				plots <- append(
					plots,
					list(mcmc_plot(
						model, 
						type=plot_types[[plot_type]], 
						pars=variables[[variable]]
						# variable=variables[[variable]], 
						# regex=TRUE
					) + ggtitle(paste(model_name, plot_type, variable)))
				)
			}
		}

		plots <- append(plots, list(pp_check(model, ndraws=100) + ggtitle(sprintf('%s PP check', model_name))))
		plots <- append(
					plots, 
					list(
						pp_check(model, ndraws=9, type='error_binned') + 
						ggtitle(sprintf('%s binned residuals PP check', model_name))
					)
				)
		
		posteriors <- as_draws_df(model, variable='^b', regex=TRUE)
		plots <- append(list(posteriors_plot(posteriors, title=sprintf('%s posteriors', model_name))), plots)
		
		# save the plots
		ggexport(
			plotlist = plots,
			filename = file.path(plots.dir, sprintf('%s_plots.pdf', gsub(' ', '_', tolower(model_name)))),
			width = 15,
			height = 12,
			scale = 0.9
		)
	}
}

# Load data
results <- read.csv('accuracy-data.csv') |>
	mutate(
		subject = as.factor(subject),
		item = as.factor(item)
	)

# set priors, following Wilson & Dillon (in prep)
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

models <- list()

# run the models
models['Crossed model'] <- do.call(brm, append(brm.args, list(
	formula = correct ~ voice.ncommand * data_source.n * target_response.n * seen_in_training.n +
		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
	data = results,
	family = bernoulli(),
	prior = priors_crossed,
	file = file.path(models.dir, 'crossed_model_accuracy.rds')
))) |> list()

# models['Crossed model (excl. clefts)'] <- do.call(brm, append(brm.args, list(
# 	formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
# 		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
# 		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item) +
# 		(1 + voice.n * data_source.n * target_response.n * seen_in_training.n | adverb),
# 	data = results |> filter(!grepl('cleft', sentence_type)),
# 	family = bernoulli(),
# 	prior = priors_crossed,
# 	file = file.path(models.dir, 'crossed_model_accuracy_excl_clefts.rds')
# ))) |> list()

models['Crossed model (non-linear)'] <- do.call(brm, append(brm.args, list(
	formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
	data = results |> filter(grepl('^Non-linear', linear)),
	family = bernoulli(),
	prior = priors_crossed,
	file = file.path(models.dir, 'crossed_model_accuracy_non-linear.rds')
))) |> list()

# models['Crossed model (non-linear, excl. clefts)'] <- do.call(brm, append(brm.args, list(
# 	formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
# 		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
# 		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item) +
# 		(1 + voice.n * data_source.n * target_response.n * seen_in_training.n | adverb),
# 	data = results |> filter(grepl('^Non-linear', linear), !grepl('cleft', sentence_type)),
# 	family = bernoulli(),
# 	prior = priors_crossed,
# 	file = file.path(models.dir, 'crossed_model_accuracy_non-linear_excl_clefts.rds')
# ))) |> list()

models['Crossed model (RTs)'] <- do.call(brm, append(brm.args, list(
	formula = RT ~ voice.n * target_response.n * seen_in_training.n +
		(1 + voice.n * target_response.n * seen_in_training.n | subject) +
		(1 | voice.n:target_response.n:seen_in_training.n:item),
	data = results |> filter(data_source == 'human') |> mutate(RT = exp(log.RT)),
	family = lognormal(),
	prior = priors_crossed_RT,
	file = file.path(models.dir, 'crossed_model_RTs.rds')
))) |> list()

save_model_summaries(
	models,
	filename=file.path(models.dir, 'model_summaries.txt'), 
	overwrite=TRUE
)
save_pmcmc(models)
save_model_plots(models)