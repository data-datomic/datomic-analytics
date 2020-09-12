package orm

type GORMTransactionObject struct {
	connectionHolder *GORMConnectionHolder
}

func NewGORMTransactionObject() *GORMTransactionObject {
	return &GORMTransactionObject{}
}

func (txObj GORMTransactionObject) SetConnectionHolder(holder *GORMConnectionHolder) {
	txObj.connectionHolder = holder
}

func (txObj GORMTransactionObject) GetConnectionHolder() *GORMConnectionHolder {
	return txObj.connectionHolder
}

func (txObj GORMTransactionObject) HasTransaction() bool {
	return txObj.connectionHolder != nil && txObj.connectionHolder.IsTransactionActive()
}
