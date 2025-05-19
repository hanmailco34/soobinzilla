package com.soobin.soobinzilla.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.request.DepartmentInsertDto;
import com.soobin.soobinzilla.dto.request.DepartmentUpdateDto;
import com.soobin.soobinzilla.dto.response.DepartmentDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;
import com.soobin.soobinzilla.exception.code.SecurityErrorCode;
import com.soobin.soobinzilla.mapper.DepartmentMapper;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.repository.department.DepartmentRepository;
import com.soobin.soobinzilla.util.ObjectUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class DepartmentService {
	
	private final DepartmentRepository departmentRepository;
	
	private final DepartmentMapper departmentMapper;
	
	public Department getById(Long id) throws FileTransferException {
		if(id == null) return null;
		Optional<Department> department = departmentRepository.findById(id);
		return department.orElseThrow(() -> new FileTransferException(DBErrorCode.NOT_FOUND));
	}
	
	public Department getByName(String name) {
		if(name == null) return null;
		Optional<Department> department = departmentRepository.findByName(name);
		return department.orElse(null);
	}
	
	public Department getByManagerId(Long managerId) {
		if(managerId == null) return null;
		Optional<Department> department = departmentRepository.findByManagerId(managerId);
		return department.orElse(null);
	}
	
	public void validMemberExists(Department department, Long id) throws FileTransferException {
		boolean exists = department.getUsers()
			.stream()
			.map(User::getId)
			.anyMatch(userId -> userId.equals(id));
		if(!exists) throw new FileTransferException(SecurityErrorCode.SECURITY_DEPARTMENT);
	}
	
	public DepartmentDto toDepartmentDto(DepartmentInsertDto requestDto) {
		return departmentMapper.toDepartmentDto(requestDto);
	}
	
	public DepartmentDto toDepartmentDto(DepartmentUpdateDto requestDto) {
		return departmentMapper.toDepartmentDto(requestDto);
	}
	
	public DepartmentDto get(Long id) throws FileTransferException {
		Department department = getById(id);
		return departmentMapper.toDepartmentDto(department);
	}

	public List<DepartmentDto> getList() {
		List<Department> departments = departmentRepository.findAll();
		return departments.stream()
				.map(departmentMapper::toDepartmentDto)
				.collect(Collectors.toList());
	}
	
	public DepartmentDto insert(DepartmentDto requestDto) throws FileTransferException {
		Department parent = getById(requestDto.getParentId());
		Department department = departmentMapper.toDepartment(requestDto, parent);
		departmentRepository.save(department);
		return departmentMapper.toDepartmentDto(department);
	}
	
	public Boolean delete(Long id) throws FileTransferException {
		Department department = getById(id);
		departmentRepository.delete(department);
		return true;
	}
	
	public void deleteChildrenAll(Long id) {
		departmentRepository.deleteChildrenByParentId(id);
	}
	
	public void updateNewParentId(Long parentId, Long newParentId) throws FileTransferException {
		getById(newParentId);
		departmentRepository.updateChildrenNewParentId(parentId, newParentId);
	}
	
	public Boolean validateUpdate(DepartmentDto requestDto) throws FileTransferException {
		getById(requestDto.getParentId());
		Department department = getByName(requestDto.getName());
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(department)) && !department.getId().equals(requestDto.getId())) throw new FileTransferException(DBErrorCode.DUPLICATE_ENTITY);
				
		Boolean result = hasCycle(requestDto.getParentId(), requestDto.getId());
		if(Boolean.TRUE.equals(result)) throw new FileTransferException(DBErrorCode.LOOP_ENTITY);
		return true;
	}
	
	public Boolean update(DepartmentDto requestDto, User manager) throws FileTransferException {
		Department parent = getById(requestDto.getParentId());
		
		Department department = getById(requestDto.getId());
		department.updateDepartment(requestDto.getName(), parent, manager); 
		departmentRepository.save(department);
		return true;
	}
	
	public void clearManager(Department department) {
		department.updateDepartment(department.getName(), department.getParent(), null);
		departmentRepository.save(department);
	}
	
	public Boolean isSubDepartment(Department child, Department parent) {
		Department current = child;
		while(current != null) {
			if (current.equals(parent)) {
                return true;
			}
			current = current.getParent();
		}
		return false;
	}
	
	public List<Department> getChildren(Department department) {
		List<Department> result = new ArrayList<>(department.getSubDepartments());
	
		for(Department child : department.getSubDepartments()) {
			List<Department> subDepartment = getChildren(child);
			result.addAll(subDepartment);
		}
		return result;
	}
	
	/**
	 * 루프 체크
	 * @param newParentId
	 * @param currentId
	 * @return
	 */
	private Boolean hasCycle(Long newParentId, Long currentId) {
		List<Department> list = departmentRepository.findAll();
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(newParentId))) return false;
		
		Set<Long> visited = new HashSet<>();
		Long parentId = newParentId;
		
		while(parentId != null) {
			if(parentId.equals(currentId) || !visited.add(parentId)) return true;
			
			Long currentParentId = parentId;
					
			parentId = list.stream()
						.filter(d -> d.getId().equals(currentParentId))
						.map(Department::getParent)
						.filter(d -> !ObjectUtil.isEmpty(d))
						.map(Department::getId)		
						.findFirst()				
						.orElse(null);
		}
		
		return false;
	}
}
