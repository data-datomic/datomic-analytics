package tx

import (
	"errors"
)

type InvokeCallback func()

func invokeWithinTransaction(txDef TransactionDefinition, txManager TransactionManager, invokeCallback InvokeCallback) (err error) {
	if invokeCallback == nil {
		err = errors.New("invoke Callback function must not be null")
		return
	}
	/* create a transaction if necessary */
	var transactionStatus TransactionStatus
	transactionStatus, err = createTransactionIfNecessary(txDef, txManager)
	if err != nil {
		return
	}
	defer func() {
		if r := recover(); r != nil {
			/* rollback transaction */
			err = errors.New("transaction couldn't be completed successfully")
			if txDef != nil && transactionStatus != nil {
				err = txManager.Rollback(transactionStatus)
			}
		}
	}()
	/* invoke function */
	invokeCallback()
	defer func() {
		if r := recover(); r != nil {
			err = errors.New("transaction couldn't be committed")
		}
	}()
	/* complete transaction */
	if txDef != nil && transactionStatus != nil {
		err = txManager.Commit(transactionStatus)
	}
	return
}

func createTransactionIfNecessary(txDef TransactionDefinition, txManager TransactionManager) (TransactionStatus, error) {
	return txManager.GetTransaction(txDef)
}
