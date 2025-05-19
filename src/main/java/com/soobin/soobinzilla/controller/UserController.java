package com.soobin.soobinzilla.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.request.UserInsertDto;
import com.soobin.soobinzilla.dto.request.UserUpdateDto;
import com.soobin.soobinzilla.dto.response.UserDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.SecurityErrorCode;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.model.enums.Role;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.AuthTokenService;
import com.soobin.soobinzilla.service.DepartmentService;
import com.soobin.soobinzilla.service.PermissionService;
import com.soobin.soobinzilla.service.UserService;
import com.soobin.soobinzilla.util.AuthUtil;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.SecurityUtil;
import com.soobin.soobinzilla.util.ValidUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "유저 컨트롤러", description = "유저 관련 API")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.USER_URL)
@RequiredArgsConstructor
public class UserController {
	
	private final UserService userService;
	
	private final AuthTokenService authTokenService;
	
	private final DepartmentService departmentService;
	
	private final PermissionService permissionService;
	
	private final ResponseHandler responseHandler;
	
	private final AuthUtil authUtil;
	
	@Tag(name = "유저 컨트롤러", description = "유저 관련 API")
	@Operation(summary = "정보얻기", description = "로그인한 유저에 대한 정보를 가져온다.")
	@GetMapping("")
	public ResponseApi<UserDto> get() throws FileTransferException {
		Long id = authUtil.getPayload().getId();
		return responseHandler.generateResponse(userService.get(id));
	}
	
	@Tag(name = "유저 컨트롤러", description = "유저 관련 API")
	@Operation(summary = "회원가입", description = "유저 회원 가입")
	@PostMapping(SecurityUtil.INSERT_URL)
	public ResponseApi<UserDto> insert(@RequestBody UserInsertDto requestDto) throws FileTransferException {
		userService.validUserId(requestDto.getUserId());
		userService.validPassword(requestDto.getPassword());
		userService.validName(requestDto.getName());
		ValidUtil.checkNull(requestDto.getDepartmentId());
		Department department = departmentService.getById(requestDto.getDepartmentId());
		UserDto userDto = userService.toUserDto(requestDto);
		userDto.setRole(Role.MEMBER);
		return responseHandler.generateResponse(userService.insert(userDto, department));
	}
	
	@Tag(name = "유저 컨트롤러", description = "유저 관련 API")
	@Operation(summary = "정보변경", description = "타입(info,password)에 따른 로그인한 유저의 이름 및 비밀번호 변경")
	@PostMapping(SecurityUtil.UPDATE_URL + "/{type}")
	public ResponseApi<UserDto> update(@RequestBody UserUpdateDto requestDto, @PathVariable String type) throws FileTransferException {
		Long id = authUtil.getPayload().getId();
		if(ConstantUtil.INFO_UPDATE.equals(type)) {
			userService.validName(requestDto.getName());
		} else if(ConstantUtil.PASSWORD_UPDATE.equals(type)) {
			userService.validPassword(requestDto.getPassword());
		} else {
			throw new FileTransferException(SecurityErrorCode.SECURITY_TYPE);
		}
		UserDto userDto = userService.toUserDto(requestDto);
		return responseHandler.generateResponse(userService.updateByType(id, userDto, type));
	}
	
	@Tag(name = "유저 컨트롤러", description = "유저 관련 API")
	@Operation(summary = "탈퇴하기", description = "로그인한 유저 탈퇴")
	@PostMapping(SecurityUtil.DELETE_URL)
	public ResponseApi<Boolean> delete() throws FileTransferException {
		Long id = authUtil.getPayload().getId();
		// 팀장 먼저 직위 해제
		Department department = departmentService.getByManagerId(id);
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(department))) departmentService.clearManager(department);
		// Permission delete
		User user = userService.getById(id);
		permissionService.deleteAllByUser(user);
		
		userService.delete(id);
		authTokenService.logout(id);
		return responseHandler.generateResponse(true);
	}
}
