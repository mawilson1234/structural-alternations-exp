source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='RTs',
	model.type='crossed',
	data.file='accuracy-data.csv',
	data.function=function(df) {
			return (df |>
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
				droplevels()
			)
		},
	formulae.file='crossed_model_formula_RTs.rds',
	formula.no=1,
	model.lists.file='model_lists_RTs.rds',
	model.no=1,
	family=lognormal()
)