package com.soobin.soobinzilla.repository.permission;

import java.util.List;

import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Permission;

public interface PermissionRepositoryCustom {
	
	public List<Permission> findAllByDepartment(Department department);
}
