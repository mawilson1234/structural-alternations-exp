source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='RTs',
	model.type='nested',
	data.file='RT-data-nested.csv',
	data.function=ident,
	formulae.file='nested_model_formulae_RTs.rds',
	formula.no=23,
	model.lists.file='model_lists_RTs.rds',
	model.no=1,
	family=lognormal()
)