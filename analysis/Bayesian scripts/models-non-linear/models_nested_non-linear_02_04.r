source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_non-linear',
	model.type='nested',
	data.file='accuracy-data-nested.csv',
	data.function=function(df) { return (df |> filter(linear == 'Non-linear') |> droplevels()) },
	formulae.file='nested_model_formulae_accuracy.rds',
	formula.no=2,
	model.lists.file='model_lists_accuracy_non-linear.rds',
	model.no=4
)