package com.soobin.soobinzilla.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.SecurityErrorCode;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.model.enums.Role;
import com.soobin.soobinzilla.util.AuthUtil;
import com.soobin.soobinzilla.util.ObjectUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ManagerService {
	
	private final DepartmentService departmentService;
	
	private final UserService userService;
	
	private final AuthUtil authUtil;

	/**
	 * 어드민은 null return
	 * @return
	 * @throws FileTransferException
	 */
	public Department getDepartment() throws FileTransferException {
		Long id = authUtil.getPayload().getId();
		Role role = authUtil.getPayload().getRole();
		
		if(Role.MEMBER.equals(role)) {
			Department department = departmentService.getByManagerId(id);
			if(Boolean.TRUE.equals(ObjectUtil.isEmpty(department))) throw new FileTransferException(SecurityErrorCode.SECURITY_FORBIDDEN);
			return department;
		}
		
		return null;
	}
	
	public List<Department> getChildrenDepartment() throws FileTransferException {
		Department department = getDepartment();
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(department))) return new ArrayList<>();
		return departmentService.getChildren(department);
	}
	
	public void isAdmin() throws FileTransferException  {
		Role role = authUtil.getPayload().getRole();
		if(Boolean.FALSE.equals(Role.ADMIN.equals(role))) throw new FileTransferException(SecurityErrorCode.SECURITY_ADMIN);
	}
	
	public void isExistMember(Long userId) throws FileTransferException {
		
		Department managerDepartment = getDepartment();
		
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(managerDepartment))) {
			departmentService.validMemberExists(managerDepartment, userId);
		}
	}
	
	public void isSubDepartmentByUser(Long userId) throws FileTransferException {
		User user = userService.getById(userId);
		isSubDepartment(user.getDepartment());
	}
	
//	public void isSubDepartmentByDepartment(Long departmentId) throws FileTransferException {
//		Department department = departmentService.getById(departmentId);
//		isSubDepartment(department);
//	}
	
	public void isSubDepartment(Department subDepartment) throws FileTransferException {
		Department managerDepartment = getDepartment();
		
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(managerDepartment)) 
				&& Boolean.FALSE.equals(departmentService.isSubDepartment(subDepartment, managerDepartment))) throw new FileTransferException(SecurityErrorCode.SECURITY_DEPARTMENT);
		
	}
}
