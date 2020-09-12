package orm

import (
	tx "github.com/procyon-projects/procyon-tx"
	"log"
	"testing"
)

func TestDoGetTransaction(t *testing.T) {
	txManager := NewGORMTransactionManager(nil, tx.NewSimpleTransactionResourcesManager())
	tx := txManager.DoGetTransaction()
	log.Print(tx)
}
