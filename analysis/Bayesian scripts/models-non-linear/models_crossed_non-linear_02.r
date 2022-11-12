source('Bayesian scripts/summary_functions.r')

fit.model(
	data.type='accuracy_non-linear',
	model.type='crossed',
	data.file='accuracy-data.csv',
	data.function=function(df) { return (df |> filter(linear == 'Non-linear')) |> droplevels() },
	formulae.file='crossed_model_formula_accuracy.rds',
	formula.no=1,
	model.lists.file='model_lists_accuracy_non-linear.rds',
	model.no=2
)