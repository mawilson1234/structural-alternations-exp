source('Bayesian scripts/summary_functions.r')
library(ggdist)
library(forcats)

CI_RANGE <- 0.95
TARGET_CI_WIDTH <- 2

N_HUMAN_PARTICIPANTS_PER_RUN <- seq(from=40, to=80, by=10)
N_RUNS_PER_SIZE <- 10
EFFECT_OF_INTEREST <- 'b_voice.n:data_source.n:target_response.n'

plots.dir <- 'Plots/Bayesian simulations'
models.dir <- 'Models/Bayesian simulations'
dir.create(plots.dir, showWarnings=FALSE, recursive=TRUE)
dir.create(models.dir, showWarnings=FALSE, recursive=TRUE)

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
	iter=6500, chains=4, cores=4,
	backend='cmdstanr', threads=threading(4),
	control=list(adapt_delta=0.99),
	seed=425, refresh=1
)

cis <- data.frame(
		model.number = rep(seq(N_RUNS_PER_SIZE), length(N_HUMAN_PARTICIPANTS_PER_RUN)),
		n.humans = rep(N_HUMAN_PARTICIPANTS_PER_RUN, each=N_RUNS_PER_SIZE),
		ci.upper = NA,
		ci.lower = NA
	) |> 
	mutate(median = NA)

human.data <- results |>
	filter(data_source == 'human')

human.subject.ids <- human.data |>
	pull(subject) |> 
	unique()

n.real.humans <- human.subject.ids |>
	length()

model.subject.ids <- results |>
	filter(data_source == 'BERT') |>
	pull(subject) |> 
	unique()

n.models <- model.subject.ids |>
	length()

for (n.participants in N_HUMAN_PARTICIPANTS_PER_RUN) {
	# randomly pull n_human_participants_per_run - n.real.humans
	# data and duplicate it
	n.diff <- n.participants - n.real.humans
	to.duplicate <- sample(human.subject.ids, size=n.diff)
	duplicated <- results |>
		filter(subject %in% to.duplicate) |>
		mutate(
			subject = as.character(subject),
			subject = paste0(subject, '_duplicated')
		)
	
	results.with.duplicates <- results |>
		mutate(subject = as.character(subject)) |>
		rbind(duplicated) |>
		mutate(subject = as.factor(subject))
	
	# generate a list of n.human.subjects integers
	# within the range of the model.ids to
	# fit the model using the same number of
	# human and computer participants
	n.human.subjects <- results.with.duplicates |> 
		filter(data_source == 'human') |>
		pull(subject) |>
		unique() |> 
		length()
	
	model.lists <- list()
	if (n.human.subjects == n.models) {
		model.lists[[1]] <- model.subject.ids
	} else {
		set.seed(425)
		model.lists <- list()
		while (!(length(unique(model.lists)) == N_RUNS) | n.human.subjects == n.models) {
			model.lists <- replicate(
				N_RUNS, 
				sort(
					sample(
						model.subject.ids, 
						size=n.human.subjects
					)
				), 
				simplify=FALSE
			)
		}
	}
	
	models <- list()
	for (i in seq_along(model.lists)) {
		model_name <- sprintf('Crossed model %02d human participants #%02d', n.participants, i)
			
		# models[model_name] <- do.call(brm, append(brm.args, list(
		# 	formula = correct ~ voice.n * data_source.n * target_response.n * seen_in_training.n +
		# 		(1 + voice.n * target_response.n * seen_in_training.n | data_source.n:subject) +
		# 		(1 + data_source.n | voice.n:target_response.n:seen_in_training.n:item),
		# 	data = results.with.duplicates |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
		# 	family = bernoulli(),
		# 	prior = priors_crossed,
		# 	file = file.path(models.dir, sprintf('crossed_model_accuracy_%02d_hp_%02d.rds', n.participants, i))
		# ))) |> list()
		
		models[model_name] <- do.call(brm, append(brm.args, list(
			formula = correct ~ voice.n +
				(1 | subject) +
				(1 | item),
			data = results.with.duplicates |> filter(data_source == 'human' | subject %in% model.lists[[i]]),
			family = bernoulli(),
			file = file.path(models.dir, sprintf('crossed_model_accuracy_%02d_hp_%02d.rds', n.participants, i))
		))) |> list()
		
		model.cis <- models[[model_name]] |>
			posterior_samples(pars=EFFECT_OF_INTEREST) |>
			median_qi(width=CI_RANGE) |>
			select(all_of(EFFECT_OF_INTEREST), .lower:.upper)
		
		cis[cis$model.number == i & cis$n.humans == n.participants,]$ci.upper <- model.cis |> pull(.upper)
		cis[cis$model.number == i & cis$n.humans == n.participants,]$ci.lower <- model.cis |> pull(.lower)
		cis[cis$model.number == i & cis$n.humans == n.participants,]$median <- model.cis[[EFFECT_OF_INTEREST]]
	}
	
	save_model_summaries(
		models,
		filename=sprintf('crossed_model_accuracy_summaries_%02d_hp.txt', n.participants), 
		overwrite=TRUE
	)
}

cis <- cis |>
	mutate(
		`Overlaps 0?` = case_when(
				((ci.upper > 0 & ci.lower > 0) | (ci.upper < 0 & ci.lower < 0)) ~ 'No overlap',
				TRUE ~ 'Overlap'
			),
		`Overlaps 0?` = fct_relevel(`Overlaps 0?`, 'Overlap', 'No overlap'),
		width = case_when(
				ci.upper - ci.lower > TARGET_CI_WIDTH ~ paste0('>', TARGET_CI_WIDTH),
				TRUE ~ paste0('<=', TARGET_CI_WIDTH)
			),
		width = fct_relevel(width, paste0('>', TARGET_CI_WIDTH), paste0('<=', TARGET_CI_WIDTH))
	)

ci.plot <- cis |>
	mutate(width = gsub('<=', '\u2264', width)) |>
	ggplot(aes(
		x=as.factor(model.number), 
		ymin=ci.lower, ymax=ci.upper, 
		color=`Overlaps 0?`,
		linetype=width
	)) +
	geom_hline(yintercept=0, color='white') +
	geom_linerange(size=1) +
	geom_point(aes(x=as.factor(model.number), y=median), cex=2.5) +
	xlab('Simulation no.') +
	scale_y_continuous(paste0(CI_RANGE*100, '% CI of ', EFFECT_OF_INTEREST)) +
	scale_linetype_discrete(paste0('Width >', TARGET_CI_WIDTH, '?')) +
	facet_grid(paste(n.humans, 'human participants') ~ .)

ggsave(
	file=file.path(plots.dir, 'crossed_model_accuracy_simulations_cis_plot.pdf'),
	plot=ci.plot,
	device='pdf',
	scale=1,
	width=10,
	height=8,
	units='in'
)

cis.summary <- cis |> 
	group_by(n.humans, `Overlaps 0?`, width) |>
	summarize(pr = n()/N_RUNS_PER_SIZE)

sink(file.path(models.dir, 'crossed_model_accuracy_simulations_cis_summary.txt'))
cis.summary
sink()