# Load libraries
library(lme4)
library(R.utils)
library(lmerTest)

# Create directories to store results
models.dir <- 'Power analysis'
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

save_model_summaries <- function(
	models = list(), 
	filename = file.path(models.dir, 'model_summaries.txt'), 
	overwrite = FALSE
){
	topsep <- paste0(paste0(rep('#', 81), collapse = ''), '\n')
	midsep <- paste0('\n', paste0(rep('-', 81), collapse = ''), '\n')
	botsep <- paste0('\n', paste0(rep('#', 81), collapse = ''), '\n')
	
	if (file.exists(filename) & !overwrite){
		cat(paste0('File "', filename, '" already exists. Use overwrite = TRUE to overwrite.\n'))
		return ()
	}
	
	if (class(models) != 'list') {
		name <- deparse(substitute(models))
		models <- list(models)
		names(models) <- name
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

re.findall <- function(regex, str) {
	# lol R is awful
	return (regmatches(str, gregexpr(regex, str, perl=TRUE))[[1]])
}

simplify.lmem.formula <- function(model) {
	# simplifies a model by removing the 
	# random effect associated with the least variance
	# if there are multiple random effects associated with
	# the same least variance, it only removes the first one
	# params: model: 	a lmem fit using the formula
	# returns: formula: a formula that removes a single random effect
	#					associated with the least variance
	
	# whatever this actually is...
	formula <- formula(model)
	
	fixed.effects <- deparse(nobars(formula))
	random.effects <- sapply(findbars(formula), deparse1)
	
	sds <- as.data.frame(VarCorr(model))
	sds <- sds[order(sds$vcov),]
	least.variance.group <- gsub('\\.[1-9]*$', '', sds[1,'grp'])
	least.variance.effect <- gsub('\\(Intercept\\)', '1', sds[1,'var1'])
	
	to.remove <- paste0(least.variance.effect, ' \\| ', least.variance.group)
	
	pre.removal.length <- length(random.effects)
	random.effects <- random.effects[!grepl(paste0('(^| )', to.remove, '$'), random.effects)]
	post.removal.length <- length(random.effects)
	if (post.removal.length != pre.removal.length - 1) {
		cat('Error: >1 random effect removed!\n')
		return ()
	}
	
	new.formula.str <- paste0(
						fixed.effects,
						' + (',
						paste0(random.effects, collapse = ') + ('),
						')'
					)
	
	# TODO: can we return this using the most compact notation instead?
	new.formula <- as.formula(new.formula.str)
	
	return (new.formula)
}

fit.lmem.until.convergence <- function(
	lmemfun = lmer, 
	formula, 
	data = NULL,
	..., 
	file.prefix = '', 
	save_each = FALSE,
	save_final = FALSE
) {
	# Repeatedly fit a (g)lmer model until convergence
	# if a model does not converge, we first attempt three control options
	# 	increasing the number of permissible iterations from 1e5 to 1e6
	# 	using optimizer 'bobyqa'
	#	both of these at once
	# if this does not lead to convergence, we Step through the 
	# random effects and eliminate ones associated
	# with 0 variance (or the least variance, if no 0s) one at a time
	# then retry these with no controls and each of the controls above
	# until the model converges with no warnings.
	# 
	# params: 	lmemfun:		the function to use to fit the model
	#							default is lmer, call fit.glmem.until.convergence instead for glmer
	#			formula: 		the formula to fit
	#			data:			the data to fit the model to
	#			...:			(passed to (g)lmer).
	# 			file.prefix:	the base filename to save the model(s) to, if saving
	#			save_each: 		save each model step as RDS?
	#			save_final:		save the final model as RDS (if not saving each)?
	# returns: 	the converged model
	
	no.output <- ifelse(Sys.info()['sysname'][[1]] == 'Windows', 'NUL', '/dev/null')
	
	save.model <- function(model, count = NULL) {
		if (!is.null(count)) {
			saveRDS(model, paste0(file.prefix, '_', sprintf('%02d', count), '.rds'))
		} else {
			saveRDS(model, paste0(file.prefix, '.rds'))
		}
	}
	
	is.converged <- function(model) {
		# returns true if the model converged
		# or false if not
		messages <- model@optinfo$conv$lme4$messages
		is.warning <- any(grepl('(failed to converge|boundary \\(singular\\) fit)', messages))
		# if (is.warning) cat(paste0(messages, collapse='\n'), '\n')
		return (!is.warning)
	}
	
	controlfun <- ifelse(identical(lmemfun, lmer), lmerControl, glmerControl)
	controlfunstr <- ifelse(identical(lmemfun, lmer), 'lmerControl', 'glmerControl')
	controlfunstr1 <- paste0(controlfunstr, '(optCtrl=list(maxfun=1e6))')
	controlfunstr2 <- paste0(controlfunstr, '(optimizer="bobyqa")')
	controlfunstr3 <- paste0(controlfunstr, '(optimizer="bobyqa", optCtrl=list(maxfun=1e6))')
	
	# before removing a source of variance
	# we try these optimizations, in this order
	controls <- list(
		controlfun(optCtrl=list(maxfun=1e6)),
		controlfun(optimizer='bobyqa'),
		controlfun(optimizer='bobyqa', optCtrl=list(maxfun=1e6))
	)
	names(controls) <- c(controlfunstr1, controlfunstr2, controlfunstr3)
	
	directory <- dirname(file.prefix)
	if (!file.exists(directory)) dir.create(directory, recursive=TRUE)
	
	# do, then while
	cat('Fitting model:\n', gsub('\\(', '\n\t\\(', gsub('\\s\\s+', ' ', paste(deparse(formula), collapse=' '))), '\n')
	model <- withCallingHandlers(lmemfun(formula=formula, data=data, ...), warning = \(w) {}, message = \(m) {})
	count <- 1
		
	if (save_each) save.model(model, count)
	count <- count + 1
	
	while (!is.converged(model)) {
		# first, try using the controls to fit the model
		# without simplifying the effects structure
		for (control.name in names(controls)) {
			cat(paste0('Reattempting same model with control=', control.name, '.\n'))
			model <- withCallingHandlers(lmemfun(formula=formula, data=data, ..., control=controls[[control.name]]), warning = \(w) {}, message = \(m) {})
			
			if (save_each) save.model(model, count)
			count <- count + 1
			
			if (is.converged(model)) break
		}
		
		# if the controls haven't helped, simplify the formula before we rerun
		if (!is.converged(model)) {
			cat('Simplifying formula.\n')
			formula <- simplify.lmem.formula(model)
			
			cat('Fitting model:\n', gsub('\\(', '\n\t\\(', gsub('\\s\\s+', ' ', paste(deparse(formula), collapse=' '))), '\n')
			model <- withCallingHandlers(lmemfun(formula=formula, data=data, ...), warning = \(w) {}, message = \(m) {})
			
			if (save_each) save.model(model, count)
			count <- count + 1
		}
	}
	
	cat('Model converged using formula:\n', gsub('\\(', '\n\t\\(', gsub('\\s\\s+', ' ', paste(deparse(formula), collapse=' '))), '\n')
	
	if (save_final & !save_each) save.model(model)
	
	return (model)
}

fit.glmem.until.convergence <- function(formula, data = NULL, ...) {
	
	return (fit.lmem.until.convergence(lmemfun=glmer, formula=formula, data=data, ...))
}

# Load data
results <- read.csv('cleaned-power-analysis-data.csv')

# run the model to get the effect sizes
crossed.model <- fit.glmem.until.convergence(
	formula = correct ~ data_source.n * voice.n * target_response.n * seen_in_training.n +
		(1 + data_source.n * voice.n * target_response.n * seen_in_training.n || subject) +
		(1 + data_source.n * voice.n * target_response.n * seen_in_training.n || item) +
		(1 + data_source.n * voice.n * target_response.n * seen_in_training.n || adverb),
	data = results,
	family = binomial(),
	verbose = 2,
	file.prefix = file.path(models.dir, 'salts_power_analysis'),
	save_each = TRUE
)

save_model_summaries(
	crossed.model, 
	filename = file.path(models.dir, 'salts_power_analysis.txt'), 
	overwrite = TRUE
)