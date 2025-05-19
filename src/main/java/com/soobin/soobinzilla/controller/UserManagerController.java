package com.soobin.soobinzilla.controller;

import java.util.List;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.dto.request.UsersDeleteDto;
import com.soobin.soobinzilla.dto.request.UsersInsertDto;
import com.soobin.soobinzilla.dto.request.UsersUpdateDto;
import com.soobin.soobinzilla.dto.response.ListDto;
import com.soobin.soobinzilla.dto.response.UserDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.model.enums.Role;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.AuthTokenService;
import com.soobin.soobinzilla.service.DepartmentService;
import com.soobin.soobinzilla.service.ManagerService;
import com.soobin.soobinzilla.service.PermissionService;
import com.soobin.soobinzilla.service.UserManagerService;
import com.soobin.soobinzilla.service.UserService;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "유저 관리 컨트롤러", description = "유저 관리 관련 API")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.USERS_URL)
@RequiredArgsConstructor
public class UserManagerController {
	
	private final UserManagerService userManagerService;
	
	private final UserService userService;
	
	private final DepartmentService departmentService;
	
	private final PermissionService permissionService;
	
	private final AuthTokenService authTokenService;
	
	private final ManagerService managerService;
	
	private final ResponseHandler responseHandler;

	@Tag(name = "유저 관리 컨트롤러", description = "유저 관리 관련 API")
	@Operation(summary = "유저 리스트 얻기(MANAGER 이상)", description = "어드민은 전체 유저, MANAGER은 본인 부서뿐만 아니라 각 하위 부서에 대한 모든 유저 리스트를 가져온다.")
	@PostMapping(SecurityUtil.LIST_URL)
	public ResponseApi<ListDto<UserDto>> list(@RequestBody PageDto requestDto) throws FileTransferException {
		Department department = managerService.getDepartment();
		List<Department> childrenDepartment = managerService.getChildrenDepartment();
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(department))) {
			childrenDepartment.add(department);
		}
		
		return responseHandler.generateResponse(userManagerService.list(requestDto, childrenDepartment));
	}
	
	@Tag(name = "유저 관리 컨트롤러", description = "유저 관리 관련 API")
	@Operation(summary = "유저 삽입하기(MANAGER 이상)", description = "어드민은 어드민 유저를 만들 수 있고, MANAGER은 하위 부서까지만 지정할 수 있다.")
	@PostMapping(SecurityUtil.INSERT_URL)
	public ResponseApi<UserDto> insert(@RequestBody UsersInsertDto requestDto) throws FileTransferException {		
		userService.validUserId(requestDto.getUserId());
		userService.validPassword(requestDto.getPassword());
		userService.validName(requestDto.getName());
		
		Department department = null;
		if(Role.ADMIN.equals(requestDto.getRole())) {
			managerService.isAdmin();
		} else {
			department = departmentService.getById(requestDto.getDepartmentId());
			managerService.isSubDepartment(department);
		}
		
		UserDto userDto = userManagerService.toUserDto(requestDto);
		
		return responseHandler.generateResponse(userManagerService.insert(userDto, department));
	}
	
	@Tag(name = "유저 관리 컨트롤러", description = "유저 관리 관련 API")
	@Operation(summary = "유저 수정하기(MANAGER 이상)", description = "ROLE 변경은 ADMIN만 할 수 있고, MANAGER은 하위 부서까지만 수정할 수 있다. 어드민으로 변경, 부서 변환시 Permission은 삭제된다.")
	@PostMapping(SecurityUtil.UPDATE_URL  + "/{type}")
	public ResponseApi<UserDto> update(@RequestBody UsersUpdateDto requestDto, @PathVariable String type) throws FileTransferException {
		User user = userService.getById(requestDto.getId());
		Department department = null;
		if(Role.ADMIN.equals(user.getRole()) || ConstantUtil.ROLE_UPDATE.equals(type)) {
			managerService.isAdmin();
			permissionService.deleteAllByUser(user);
		} else {
			department = user.getDepartment();
			managerService.isSubDepartment(department);
			if(ConstantUtil.DEPARTMENT_UPDATE.equals(type)) {
				department = departmentService.getById(requestDto.getDepartmentId());
				managerService.isSubDepartment(department);
				permissionService.deleteAllByUser(user);
			}
		}
		
		if(ConstantUtil.PASSWORD_UPDATE.equals(type)) {
			userService.validPassword(requestDto.getPassword());
		} else if(ConstantUtil.INFO_UPDATE.equals(type)) {
			userService.validName(requestDto.getName());
		}
		
		UserDto userDto = userManagerService.toUserDto(requestDto);
		
		return responseHandler.generateResponse(userManagerService.update(user, userDto, department, type));
	}
	
	@Tag(name = "유저 관리 컨트롤러", description = "유저 관리 관련 API")
	@Operation(summary = "유저 삭제하기(MANAGER 이상)", description = "어드민은 어드민을 삭제시킬 수 있고, MANAGER은 하위 부서까지만 삭제할 수 있다.")
	@PostMapping(SecurityUtil.DELETE_URL)
	public ResponseApi<Boolean> delete(@RequestBody UsersDeleteDto requestDto) throws FileTransferException {
		User user = userService.getById(requestDto.getId());
		if(Role.ADMIN.equals(user.getRole())) {
			managerService.isAdmin();
		} else {
			managerService.isSubDepartment(user.getDepartment());
		}
		// 팀장 먼저 직위 해제
		Department department = departmentService.getByManagerId(requestDto.getId());
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(department))) departmentService.clearManager(department);
		
		permissionService.deleteAllByUser(user);
		
		userService.delete(requestDto.getId());
		authTokenService.logout(requestDto.getId());
		return responseHandler.generateResponse(true);
	}
}
