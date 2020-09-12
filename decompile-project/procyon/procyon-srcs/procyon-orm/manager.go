package orm

import (
	"github.com/jinzhu/gorm"
	tx "github.com/procyon-projects/procyon-tx"
	"log"
)

type GORMTransactionManager struct {
	tx.TransactionManager
	resourceManager   tx.TransactionResourcesManager
	connectionFactory *GORMConnectionFactory
}

func NewGORMTransactionManager(connectionFactory *GORMConnectionFactory, resourceManager tx.TransactionResourcesManager) *GORMTransactionManager {
	txMgr := &GORMTransactionManager{
		resourceManager:   resourceManager,
		connectionFactory: connectionFactory,
	}
	abstractTxMgr := tx.NewAbstractTransactionManager(txMgr)
	txMgr.TransactionManager = abstractTxMgr
	return txMgr
}

func (txManager *GORMTransactionManager) DoGetTransaction() interface{} {
	txObject := NewGORMTransactionObject()
	connectionHolder := txManager.resourceManager.GetResource(txManager.connectionFactory)
	if connectionHolder != nil {
		txObject.SetConnectionHolder(connectionHolder.(*GORMConnectionHolder))
	}
	return txObject
}

func (txManager *GORMTransactionManager) DoBeginTransaction(txObj interface{}, txDef tx.TransactionDefinition) {
	txObject := txObj.(*GORMTransactionObject)
	if txObject.GetConnectionHolder() != nil {
		panic("There is already a running transaction")
	}
	connection := txManager.connectionFactory.CreateConnection()
	connectionHolder := NewGORMConnectionHolder(connection.(*GORMConnection))
	txObject.SetConnectionHolder(connectionHolder)
	txManager.resourceManager.BindResource(txManager.connectionFactory, connectionHolder)
}

func (txManager *GORMTransactionManager) DoSuspendTransaction(txObj interface{}) interface{} {
	txObject := txObj.(*GORMTransactionObject)
	txObject.SetConnectionHolder(nil)
	/* unbind old connection resource */
	connectionHolder := txManager.resourceManager.UnBindResource(txManager.connectionFactory)
	return NewGORMSuspendedResources(connectionHolder.(*GORMConnectionHolder))
}

func (txManager *GORMTransactionManager) DoResumeTransaction(txObj interface{}, txSuspendedResources interface{}) {
	resources := txSuspendedResources.(*GORMSuspendedResources)
	if resources != nil && resources.GetConnectionHolder() != nil {
		txManager.resourceManager.BindResource(txManager.connectionFactory, resources.GetConnectionHolder())
	}
}

func (txManager *GORMTransactionManager) DoCommitTransaction(txStatus tx.TransactionStatus) {
	txObject := txStatus.GetTransaction().(*GORMTransactionObject)
	connectionHolder := txObject.GetConnectionHolder()
	dbConnection := connectionHolder.GetConnection().GetDBConnection().(*gorm.DB)
	defer func() {
		if r := recover(); r != nil {
			log.Print("Could not commit transaction")
		}
	}()
	dbConnection.Commit()
}

func (txManager *GORMTransactionManager) DoRollbackTransaction(txStatus tx.TransactionStatus) {
	txObject := txStatus.GetTransaction().(*GORMTransactionObject)
	connectionHolder := txObject.GetConnectionHolder()
	dbConnection := connectionHolder.GetConnection().GetDBConnection().(*gorm.DB)
	defer func() {
		if r := recover(); r != nil {
			log.Print("Could not rollback transaction")
		}
	}()
	dbConnection.Rollback()
}

func (txManager *GORMTransactionManager) IsExistingTransaction(txObj interface{}) bool {
	return txObj.(*GORMTransactionObject).HasTransaction()
}

func (txManager *GORMTransactionManager) SupportsPropagation(propagation tx.TransactionPropagation) bool {
	if propagation == tx.PropagationNested {
		return false
	}
	return true
}
