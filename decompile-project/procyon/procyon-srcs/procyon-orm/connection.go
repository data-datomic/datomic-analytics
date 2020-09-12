package orm

import "github.com/jinzhu/gorm"

type Connection interface {
	GetDBConnection() interface{}
}

type GORMConnection struct {
	db *gorm.DB
}

func NewGORMDatabaseConnection(db *gorm.DB) *GORMConnection {
	return &GORMConnection{
		db,
	}
}

func (connection *GORMConnection) GetDBConnection() interface{} {
	return connection.db
}
