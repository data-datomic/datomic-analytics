package orm

type ConnectionHolder interface {
	GetConnection() Connection
	HasConnection() bool
	IsTransactionActive() bool
}

type GORMConnectionHolder struct {
	connection        *GORMConnection
	transactionActive bool
}

func NewGORMConnectionHolder(connection *GORMConnection) *GORMConnectionHolder {
	return &GORMConnectionHolder{
		connection,
		false,
	}
}

func (holder *GORMConnectionHolder) GetConnection() Connection {
	return holder.connection
}

func (holder *GORMConnectionHolder) HasConnection() bool {
	return holder.connection != nil
}

func (holder *GORMConnectionHolder) IsTransactionActive() bool {
	return holder.transactionActive
}
