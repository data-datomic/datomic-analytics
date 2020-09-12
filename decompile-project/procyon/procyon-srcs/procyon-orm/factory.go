package orm

type ConnectionFactory interface {
	CreateConnection() Connection
}

type GORMConnectionFactory struct {
}

func NewGORMConnectionFactory() *GORMConnectionFactory {
	return &GORMConnectionFactory{}
}

func (factory *GORMConnectionFactory) CreateConnection() Connection {
	return NewGORMDatabaseConnection(nil)
}
