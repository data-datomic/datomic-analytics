package tx

type TransactionPropagation int

const (
	PropagationRequired TransactionPropagation = iota
	PropagationSupports
	PropagationMandatory
	PropagationNever
	PropagationNotSupported
	PropagationNested
	PropagationRequiredNew
)
