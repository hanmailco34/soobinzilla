package com.soobin.soobinzilla.repository.user;

import java.util.List;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.model.Department;
import com.querydsl.core.types.dsl.BooleanExpression;

public interface UserRepositotyCustom {
	Long updateAllDepartment(Long departmentId, Department department);
	BooleanExpression search(PageDto dto);
	BooleanExpression search(PageDto dto, List<Department> departments);
}
