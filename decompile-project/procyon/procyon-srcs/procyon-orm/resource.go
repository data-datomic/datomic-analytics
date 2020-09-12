package orm

type GORMSuspendedResources struct {
	connectionHolder *GORMConnectionHolder
}

func NewGORMSuspendedResources(holder *GORMConnectionHolder) *GORMSuspendedResources {
	return &GORMSuspendedResources{
		holder,
	}
}

func (resources *GORMSuspendedResources) GetConnectionHolder() *GORMConnectionHolder {
	return resources.connectionHolder
}
