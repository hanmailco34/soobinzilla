package com.soobin.soobinzilla.mapper;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.dto.request.DepartmentInsertDto;
import com.soobin.soobinzilla.dto.request.DepartmentUpdateDto;
import com.soobin.soobinzilla.dto.response.DepartmentDto;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.util.ObjectUtil;

@Component
public class DepartmentMapper {

	public DepartmentDto toDepartmentDto(Department department) {
		DepartmentDto dto = new DepartmentDto();
		dto.setId(department.getId());
		dto.setName(department.getName());
		Long managerId = Boolean.TRUE.equals(ObjectUtil.isEmpty(department.getManager())) ? null :  department.getManager().getId();
		dto.setManagerId(managerId);
		Long partentId = Boolean.TRUE.equals(ObjectUtil.isEmpty(department.getParent())) ? null :  department.getParent().getId();
		dto.setParentId(partentId);
		return dto;
	}
	
	public DepartmentDto toDepartmentDto(DepartmentInsertDto requestDto) {
		return DepartmentDto.builder()
				.name(requestDto.getName())
				.managerId(requestDto.getManagerId())
				.parentId(requestDto.getParentId())
				.build();
	}
	
	public DepartmentDto toDepartmentDto(DepartmentUpdateDto requestDto) {
		return DepartmentDto.builder()
				.id(requestDto.getId())
				.name(requestDto.getName())
				.managerId(requestDto.getManagerId())
				.parentId(requestDto.getParentId())
				.build();
	}
	
	public Department toDepartment(DepartmentDto requestDto, Department parent) {
		return Department.builder()
				.name(requestDto.getName())
				.parent(parent)
				.build();
	}
}
