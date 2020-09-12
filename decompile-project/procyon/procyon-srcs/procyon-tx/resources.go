package tx

import (
	"errors"
	"sync"
)

type TransactionResources interface {
	AddResource(key interface{}, resource interface{})
	ContainsResource(key interface{}) bool
	GetResource(key interface{}) interface{}
	RemoveResource(key interface{}) interface{}
}

type SimpleTransactionResources struct {
	resources map[interface{}]interface{}
	mu        sync.RWMutex
}

func NewSimpleTransactionResources() *SimpleTransactionResources {
	return &SimpleTransactionResources{
		resources: make(map[interface{}]interface{}),
		mu:        sync.RWMutex{},
	}
}

func (tr SimpleTransactionResources) AddResource(key interface{}, resource interface{}) {
	tr.mu.Lock()
	tr.resources[key] = resource
	tr.mu.Unlock()
}

func (tr SimpleTransactionResources) ContainsResource(key interface{}) bool {
	tr.mu.Lock()
	if _, ok := tr.resources[key]; ok {
		return true
	}
	tr.mu.Unlock()
	return false
}

func (tr SimpleTransactionResources) GetResource(key interface{}) interface{} {
	var result interface{}
	tr.mu.Lock()
	if resource, ok := tr.resources[key]; ok {
		result = resource
	}
	tr.mu.Unlock()
	return result
}

func (tr SimpleTransactionResources) RemoveResource(key interface{}) interface{} {
	var result interface{}
	tr.mu.Lock()
	if resource, ok := tr.resources[key]; ok {
		result = resource
		delete(tr.resources, key)
	}
	tr.mu.Unlock()
	return result
}

type TransactionResourcesManager interface {
	GetResource(key interface{}) interface{}
	BindResource(key interface{}, resource interface{}) error
	UnBindResource(key interface{}) (interface{}, error)
}

type SimpleTransactionResourcesManager struct {
	resources TransactionResources
}

func NewSimpleTransactionResourcesManager() SimpleTransactionResourcesManager {
	return SimpleTransactionResourcesManager{
		NewSimpleTransactionResources(),
	}
}

func (resourceManager SimpleTransactionResourcesManager) GetResource(key interface{}) interface{} {
	resources := resourceManager.resources
	if resources == nil {
		return nil
	}
	return resources.GetResource(key)
}

func (resourceManager SimpleTransactionResourcesManager) BindResource(key interface{}, value interface{}) error {
	resources := resourceManager.resources
	if resources == nil {
		return errors.New("transactional context resources must not be nil")
	}
	if resources.ContainsResource(key) {
		return errors.New("there is already added resource with same key in transactional context")
	}
	resources.AddResource(key, value)
	return nil
}

func (resourceManager SimpleTransactionResourcesManager) UnBindResource(key interface{}) (interface{}, error) {
	resources := resourceManager.resources
	if resources == nil {
		return nil, errors.New("transactional context resources must not be nil")
	}
	if !resources.ContainsResource(key) {
		return nil, errors.New("there is no resource for given key in transactional context")
	}
	return resources.RemoveResource(key), nil
}
