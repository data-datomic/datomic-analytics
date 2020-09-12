package tx

import (
	"errors"
	"github.com/google/uuid"
	"github.com/procyon-projects/procyon-core"
	"sync"
)

var (
	transactionContextPool sync.Pool
)

func initTransactionalContextPool() {
	transactionContextPool = sync.Pool{
		New: newSimpleTransactionalContext,
	}
}

type TransactionalContext interface {
	TransactionalBlock
	GetContextId() uuid.UUID
	GetTransactionManager() TransactionManager
	GetTransactionResourcesManager() TransactionResourcesManager
}

type SimpleTransactionalContext struct {
	contextId               uuid.UUID
	logger                  core.Logger
	transactionManager      TransactionManager
	transactionResourcesMgr TransactionResourcesManager
}

func newSimpleTransactionalContext() interface{} {
	return &SimpleTransactionalContext{}
}

func NewSimpleTransactionalContext(contextId uuid.UUID,
	logger core.Logger,
	transactionManager TransactionManager,
	transactionResourcesManager TransactionResourcesManager) (*SimpleTransactionalContext, error) {
	if logger == nil {
		return nil, errors.New("logger must not be nil")
	}
	if transactionManager == nil {
		return nil, errors.New("transaction manager must not be nil")
	}
	if transactionResourcesManager == nil {
		return nil, errors.New("transaction resource Manager must not be nil")
	}
	transactionalContext := transactionContextPool.Get().(*SimpleTransactionalContext)
	transactionalContext.contextId = contextId
	transactionalContext.logger = logger
	transactionalContext.transactionManager = transactionManager
	transactionalContext.transactionResourcesMgr = transactionResourcesManager
	return transactionalContext, nil
}

func (tContext *SimpleTransactionalContext) Block(fun TransactionalFunc, options ...TransactionBlockOption) error {
	if fun == nil {
		return errors.New("transaction function must not be nil")
	}
	txBlockObject := NewTransactionBlockObject(fun, options...)
	/* convert tx block object into tx block definition */
	txBlockDef := NewSimpleTransactionDefinition(
		WithTxPropagation(txBlockObject.propagation),
		WithTxReadOnly(txBlockObject.readOnly),
		WithTxTimeout(txBlockObject.timeOut),
	)
	/* invoke within transaction */
	return invokeWithinTransaction(txBlockDef, tContext.GetTransactionManager(), func() {
		txFunc := txBlockObject.fun
		txFunc()
	})
}

func (tContext *SimpleTransactionalContext) GetContextId() uuid.UUID {
	return tContext.contextId
}

func (tContext *SimpleTransactionalContext) GetTransactionManager() TransactionManager {
	return tContext.transactionManager
}

func (tContext *SimpleTransactionalContext) GetTransactionResourcesManager() TransactionResourcesManager {
	return tContext.transactionResourcesMgr
}

func (tContext *SimpleTransactionalContext) PutToPool() {
	transactionContextPool.Put(tContext)
}
