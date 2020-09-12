package tx

type TransactionalFunc func()
type TransactionBlockOption func(txBlockObj *TransactionBlockObject)

type TransactionBlockObject struct {
	fun         TransactionalFunc
	propagation TransactionPropagation
	readOnly    bool
	timeOut     int
}

func NewTransactionBlockObject(fun TransactionalFunc, options ...TransactionBlockOption) *TransactionBlockObject {
	obj := &TransactionBlockObject{
		fun,
		PropagationRequired,
		false,
		TransactionMinTimeout,
	}
	for _, opt := range options {
		opt(obj)
	}
	return obj
}

func (txBlockObj *TransactionBlockObject) GetTransactionFunc() TransactionalFunc {
	return txBlockObj.fun
}

func (txBlockObj *TransactionBlockObject) GetPropagation() TransactionPropagation {
	return txBlockObj.propagation
}

func (txBlockObj *TransactionBlockObject) GetTimeOut() int {
	return txBlockObj.timeOut
}

func (txBlockObj *TransactionBlockObject) IsReadOnly() bool {
	return txBlockObj.readOnly
}

type TransactionalBlock interface {
	Block(fun TransactionalFunc, options ...TransactionBlockOption) error
}

func WithPropagation(propagation TransactionPropagation) TransactionBlockOption {
	return func(txBlockObj *TransactionBlockObject) {
		txBlockObj.propagation = propagation
	}
}

func WithTimeout(timeOut int) TransactionBlockOption {
	return func(txBlockObj *TransactionBlockObject) {
		txBlockObj.timeOut = timeOut
	}
}

func WithReadOnly(readOnly bool) TransactionBlockOption {
	return func(txBlockObj *TransactionBlockObject) {
		txBlockObj.readOnly = readOnly
	}
}
