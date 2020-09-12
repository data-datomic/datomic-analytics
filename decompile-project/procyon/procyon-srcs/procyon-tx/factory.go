package tx

type TransactionManagerFactory interface {
	GetTransactionManager() TransactionManager
}

type DefaultTransactionManagerFactory struct {
}

func NewTransactionManagerFactory() *DefaultTransactionManagerFactory {
	return &DefaultTransactionManagerFactory{}
}

func (factory DefaultTransactionManagerFactory) GetTransactionManager() TransactionManager {
	return nil
}
