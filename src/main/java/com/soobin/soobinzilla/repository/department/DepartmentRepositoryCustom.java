package com.soobin.soobinzilla.repository.department;

public interface DepartmentRepositoryCustom {
	Long deleteChildrenByParentId(Long parentId);
	
	Long updateChildrenNewParentId(Long parentId, Long newParentId);
}
