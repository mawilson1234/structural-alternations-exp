# Load libraries
suppressMessages(library(brms))
suppressMessages(library(tools))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(ggpubr))
suppressMessages(library(stringr))
suppressMessages(library(R.utils))
suppressMessages(library(bayesplot))

# brms will not accept essentially any
# symbol in a variable name, so we're
# stuck with this. it also won't 
# take double underscores.
NESTING_SEPARATOR <- '_i_n_'
LEVEL_SEPARATOR <- '_a_t_'

# for filtering RTs
MAX_RT_IN_SECONDS <- 10
OUTLIER_RT_SDS <- 2

# Create directories to store results
plots.dir <- 'Plots/Bayesian'
models.dir <- 'Models/Bayesian'
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

bayesplot_theme_set(theme_default(base_family = getOption('bayesplot.base_family', 'sans')))

brm.args <- list(
	iter=6500, 
	chains=4, 
	cores=4,
	refresh=650,
	backend='cmdstanr', 
	threads=threading(4, static=TRUE),
	control=list(adapt_delta=0.99),
	seed=425
)

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
		labels <- gsub('\\.n(_|:|$)', '\\1', labels)
		labels <- gsub(NESTING_SEPARATOR, ' \U03F5 ', labels)
		labels <- gsub(LEVEL_SEPARATOR, ' @ ', labels)
		labels <- gsub('(\\.|_)', ' ', labels)
		labels <- gsub(':', ' × ', labels)
		labels <- toTitleCase(labels)
		labels <- gsub('Svo', 'SVO', labels)
		labels <- gsub('Ovs', 'OVS', labels)
		labels <- gsub(' Tr ', ' T.R. ', labels)
		labels <- gsub(' Ds ', ' D.S. ', labels)
		labels <- gsub(' Sit ', ' S.i.T. ', labels)
		labels <- gsub(' v ', ' V. ', labels)
		labels <- gsub(' l ', ' L. ', labels)
		labels <- gsub('Bert', 'BERT', labels)
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
	}
	
	withOptions(
		{
			for (model_name in names(models)){
				model <- models[[model_name]]
				
				# posteriors <- as_draws_df(model, variable=variable, regex=regex)
				posteriors <- posterior_samples(model, pars=variable, fixed=(!regex))
				
				summary <- posteriors |>
					# select(-`.chain`, -`.iteration`, -`.draw`) |>
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

save_model_plots <- function(models = list(), plots.dir = '.') {	
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
		
		# posteriors <- as_draws_df(model, variable='^b', regex=TRUE)
		posteriors <- posterior_samples(model, pars='^b')
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

get_nested_data <- function(data, cols, gcols, out_of_group_value = 0) {	
	for (c in cols) {
		other_cols <- gcols[which(cols != c)]
		cat(sprintf('Getting nestings for %s within levels of %s', c, paste0(other_cols, collapse=', ')), '\n')
		
		comb <- lapply(
				seq_along(other_cols),
				\(i) combn(
					other_cols,
					m=i
				)
			)
		
		comb <- lapply(
					comb,
					\(x) {
						x <- as.matrix(x)
						lapply(
							seq_len(ncol(x)),
							\(i) x[,i]
						)
					}
				)
		
		comb <- flatten(comb)
		
		for (co in comb) {
			cat(sprintf('Working on %s', paste0(co, collapse=' X ')), '\n')
			groups <- data |>
				select_at(co) |>
				distinct()
			
			for (i in seq_len(nrow(groups))) {
				group <- groups[i,]
				# R/brms won't accept any non-word character in a variable name except for a dot or a single underscore
				group_s <- tolower(gsub('(?:(?![.])([[:punct:]]| ))+', '_', paste(paste0(gsub('(\\w)(.*?)(_|$)', '\\1', names(groups))), group, sep=LEVEL_SEPARATOR, collapse=NESTING_SEPARATOR), perl=TRUE))
				group_s <- remove.double.underscores(group_s)
				# brms won't take trailing underscores in variable names
				group_s <- gsub('_$', '', group_s)
				cat(sprintf(paste0('Working on group nesting %s', NESTING_SEPARATOR, '%s'), c, group_s), '\n')
				nested_name <- paste0(c, NESTING_SEPARATOR, group_s)
				
				data <- data |> 
					rowwise() |>
					mutate(
						`__tmp__` = tolower(gsub('(?:(?![.])([[:punct:]])| )+', '_', paste(gsub('(\\w)(.*?)(_|$)', '\\1', names(groups)), c(!!!rlang::syms(names(groups))), sep=LEVEL_SEPARATOR, collapse=NESTING_SEPARATOR), perl=TRUE)) |>
							remove.double.underscores() %>%
							gsub('_$', '', .),
						'{nested_name}' := case_when(
							`__tmp__` == group_s ~ !!!rlang::syms(c),
							TRUE ~ out_of_group_value
						)
					) |> 
					select(-`__tmp__`)
			}
		}
		cat('\n')
	}
	
	return (data)
}

remove.double.underscores <- function(s) {
	while ('__' %in% s) {
		s <- gsub('__', '_', s)
	}
	return (s)
}

re.findall <- function(regex, str) {
	# lol R is awful
	return (regmatches(str, gregexpr(regex, str, perl=TRUE)))
}

get.nested.model.formulae <- function(all.nested.effects, all.effect.cols, depvar, ranefs, ranef.nestings = list()) {
	# gets formulae for all nestings of predictors in all.nested effects
	# this may not work for other designs, but it works for ours! 
	#
	# good luck if you want to change this, string manipulation in R is not fun
	# 
	# 	params:
	#  		all.nested.effects (string vector): a vector of strings contained all of the nested effects
	#											these are the column names added by get_nested_data
	#		depvar (string)					  : the name of the dependent variable
	#		ranefs (string vector)			  : the names of the random effects groups
	# 		ranef.nestings (list[str,str])	  : a list mapping random effects to
	#											the variables within which they are nested
	#
	#   returns:
	#		list[str,str]					  : a list mapping abbreviated group names
	#										    to the formula for that nested model
	#
	nested.model.formulae <- data.frame(effect = all.nested.effects) |>
		as_tibble() |> 
		filter(grepl(NESTING_SEPARATOR, effect, fixed=TRUE)) |>
		mutate(
			n.effects = str_count(effect, NESTING_SEPARATOR)
		) |>
		arrange(n.effects, effect) |>
		rowwise() |>
		mutate(
			nested = re.findall(paste0('^(.*?)(?=', NESTING_SEPARATOR, ')'), effect)[[1]],
			group = paste0(re.findall(paste0('(?<=', NESTING_SEPARATOR, ')(.*?)(?=', LEVEL_SEPARATOR, '|$)'), effect)[[1]], collapse=NESTING_SEPARATOR),
			added.effects = all.effect.cols[
								grepl(gsub(NESTING_SEPARATOR, '|', group), gsub('(\\w)(.*?)(_|$)', '\\1', all.effect.cols))
							] |>
							paste0(collapse=' * '),
			other.effects = all.effect.cols[
								!grepl(nested, all.effect.cols) & 
								!grepl(gsub(NESTING_SEPARATOR, '|', group), gsub('(\\w)(.*?)(_|$)', '\\1', all.effect.cols))
							] |>
							paste0(collapse=' * ')
		) |> 
		ungroup() |> 
		group_by(nested, n.effects, group) |>
		mutate(
			fixef = paste0(
						paste0(
							'(',
								paste0(
									paste0('`', paste0(effect, collapse='` + `'), '`'),
									ifelse(unique(other.effects) != '', paste0(' + `', gsub('( \\* | \\+ |:)', '`\\1`', unique(added.effects)), '`'), '')
								),
							')'
						),
						ifelse(unique(other.effects) != '', paste0(' * `', gsub('( \\* | \\+ |:)', '`\\1`', unique(other.effects)), '`'), '')
					)
		)
	
	for (ranef in names(ranef.nestings)) {
		ranef.nesting <- ranef.nestings[[ranef]]
		
		nested.model.formulae <- nested.model.formulae |>
			group_by(nested, n.effects, group) |>						
			mutate(
				'ranef_{ranef}' := paste0(
									'(1',
									# get the things that aren't nested in random effects
									# that are nested fixed effects
									ifelse(
											length(
												effect[
													!grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), effect)
												]
											) != 0,
											paste0(
												' + (`',
													paste0(effect[!grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), effect)], collapse='` + `'),
												'`)'
											),
											''
									),
									# get the things that aren't nested in random effects
									# that aren't nested fixed effects
									ifelse(
										length(
											strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
												!grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
											]
										) != 0,
										paste0(
											' + `',
											paste0(
												strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
													!grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
												],
												collapse = '` * `'
											),
											'`'
										),
										''
									),
									' | ',
										ifelse(
											(
												length(
												strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
													grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
												]
												) != 0
											) & (
												length(
													effect[
														grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), effect)
													]
												) != 0
											),
											'(',
											''
										),
										# get the things that are nested in random effects
										# that are nested fixed effects
										ifelse(
											length(
												effect[
													grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), effect)
												]
											) != 0,
											paste0(
												'(`',
													paste0(effect[grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), effect)], collapse='` + `'),
												'`)',
												ifelse(
													length(
														strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
															grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
														]
													) != 0,
													':',
													''
												)
											),
											''
											# paste0(paste0(ranef.nesting, collapse=':'), ':')
										),
										# get the things that are nested in random effects
										# that aren't nested fixed effects
										ifelse(
											length(
												strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
													grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
												]
											) != 0,
											paste0(
												'`',
												paste0(
													strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
														grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
													],
													collapse = '`:`'
												),
												'`',
												ifelse(
													(
														length(
														strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]][
															grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), strsplit(paste0(unique(added.effects), ' * ', unique(other.effects)), ' \\* ', perl=TRUE)[[1]])
														]
														) != 0
													) & (
														length(
															effect[
																grepl(paste0('^(', paste0(ranef.nesting, collapse='|'), ')'), effect)
															]
														) != 0
													),
													'):`',
													':`'
												),
												''
											),
											':`'
										),
									ranef,
									'`)'
								)
			)
	}
	
	ranef.cols <- colnames(nested.model.formulae)[grepl('^ranef_', colnames(nested.model.formulae))]
	
	nested.model.formulae <- nested.model.formulae |>
		unite('ranef', all_of(ranef.cols), sep=' + ') |>
		group_by(nested, n.effects, group) |>
		mutate(
			depvar = depvar,
			formula = paste0(
						'`', unique(depvar), '`',
						' ~ ',
						unique(fixef),
						' + ',
						unique(ranef)
					)
		) |>
		ungroup() |>
		select(nested, group, formula) |>
		distinct()
		
		# 	# TODO: figure out how to do the nested random effects
		# 	#		with the nested fixed effects. is this right?
		# 	ranef = paste0(
		# 				paste0(
		# 					'(1 + ',
		# 					unique(fixef),
		# 					' | ',
		# 					ranefs,
		# 					collapse = ') + '
		# 				),
		# 				')'
		# 			)
		# }
		# 	depvar = depvar,
		# 	formula = paste0(
		# 				unique(depvar),
		# 				' ~ ',
		# 				unique(fixef),
		# 				' + ',
		# 				unique(ranef)
		# 			)
		# ) |>
		# ungroup() |>
		# select(nested, group, formula) |> 
		# distinct()
	
	nested.model.formulae.list <- lapply(nested.model.formulae$formula, formula)
	names(nested.model.formulae.list) <- paste0(nested.model.formulae$nested, NESTING_SEPARATOR, nested.model.formulae$group)
	
	return (nested.model.formulae.list)
}

ident <- function(df) return (df)

fit.model <- function(
	data.type,
	model.type, 
	data.file,
	data.function,
	formulae.file,
	formula.no,
	model.lists.file,
	model.no,
	family=bernoulli()
) {
	models.dir <- file.path('Models', 'Bayesian', data.type)
	plots.dir <- file.path('Plots', 'Bayesian', data.type)
	dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)
	dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
	
	results <- read.csv(data.file) |> 
		mutate(
			subject = as.factor(subject),
			item 	= as.factor(item)
		) |>
		data.function()
	
	model.formulae <- readRDS(file.path('Bayesian scripts', formulae.file))
	name <- names(model.formulae)[[formula.no]]
	
	formula <- model.formulae[[name]]
	effects <- attr(terms(formula), 'term.labels')
	fixef <- effects[!grepl('^1|0 + ', effects)]
	priors <- c(
		set_prior('normal(0, 10)', class='Intercept'),
		set_prior('lkj(2)', class='cor'),
		set_prior('normal(0, 1)', class = 'b', coef=fixef)
	)
	
	models <- list()
	i <- model.no
	model.lists <- readRDS(file.path('Bayesian scripts', model.lists.file))
	
	if (model.type == name) {
		model.dir <- file.path(models.dir, paste0(model.type))
		plot.dir <- file.path(plots.dir, paste0(model.type))
		if (length(model.lists) == 1) {
			out.file <- sprintf('%s_model_%s', model.type, data.type)
			info.str <- sprintf('%s model (%s)', model.type, data.type)
			model.name <- sprintf('%s model (%s)', toTitleCase(model.type), data.type)
		} else {
			out.file <- sprintf('%s_model_%s_%02d', model.type, data.type, i)
			info.str <- sprintf('%s model (%s) %02d', model.type, data.type, i)
			model.name <- sprintf('%s model (%s) %02d', toTitleCase(model.type), data.type, i)
		}
	} else {
		model.dir <- file.path(models.dir, paste0(model.type, '_', name))
		plot.dir <- file.path(plots.dir, paste0(model.type, '_', name))
		if (length(model.lists) == 1) {
			out.file <- sprintf('%s_model_%s_%s', model.type, data.type, name)
			info.str <- sprintf('%s model (%s) %s', model.type, data.type, name)
			model.name <- sprintf('%s model (%s) %s', toTitleCase(model.type), data.type, name)
		} else {
			out.file <- sprintf('%s_model_%s_%s_%02d', model.type, data.type, name, i)
			info.str <- sprintf('%s model (%s) %s %02d', model.type, data.type, name, i)
			model.name <- sprintf('%s model (%s) %s %02d', toTitleCase(model.type), data.type, name, i)
		}
	}
	
	dir.create(model.dir, showWarnings=FALSE, recursive=TRUE)
	dir.create(plot.dir, showWarnings=FALSE, recursive=TRUE)
	
	if (file.exists(file.path(model.dir, paste0(out.file, '.rds')))) {
		cat('Loading ', info.str, '\n', sep='')
	} else {
		cat('Fitting ', info.str, '\n', sep='')
	}
	
	# debug
	# cat(paste0('out.file: ', out.file), '\n')
	# cat(paste0('model.dir: ', model.dir), '\n')
	# cat(paste0('plot.dir: ', plot.dir), '\n')
	# quit()
	
	models[model.name] <- do.call(brm, append(brm.args, list(
		formula = formula,
		data = results |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
		family = family,
		prior = priors,
		file = file.path(model.dir, paste0(out.file, '.rds'))
	))) |> list()
	
	save_model_summaries(
		models,
		filename=file.path(model.dir, paste0(out.file, '_summary.txt')),
		overwrite=TRUE
	)

	save_pmcmc(
		models,
		filename=file.path(model.dir, paste0(out.file, '_pmcmcs.txt'))
	)

	save_model_plots(
		models,
		plots.dir=plot.dir
	)
}
