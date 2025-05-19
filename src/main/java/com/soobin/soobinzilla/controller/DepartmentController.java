package com.soobin.soobinzilla.controller;

import java.util.List;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.request.DepartmentDeleteDto;
import com.soobin.soobinzilla.dto.request.DepartmentInsertDto;
import com.soobin.soobinzilla.dto.request.DepartmentUpdateDto;
import com.soobin.soobinzilla.dto.response.DepartmentDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.DepartmentService;
import com.soobin.soobinzilla.service.ScheduleService;
import com.soobin.soobinzilla.service.UserService;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "부서 컨트롤러", description = "부서 관련 API")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.DEPARTMENT_URL)
@RequiredArgsConstructor
public class DepartmentController {
	
	private final DepartmentService departmentService;
	
	private final UserService userService;
	
	private final ScheduleService scheduleService;
	
	private final ResponseHandler responseHandler;
	
	@Tag(name = "부서 컨트롤러", description = "부서 관련 API")
	@Operation(summary = "부서 상세 정보 조회", description = "지정된 ID를 사용하여 부서의 상세 정보를 조회합니다.")
	@GetMapping("/{id}")
	public ResponseApi<DepartmentDto> get(@PathVariable Long id) throws FileTransferException {
		return responseHandler.generateResponse(departmentService.get(id));
	}
	
	@Tag(name = "부서 컨트롤러", description = "부서 관련 API")
	@Operation(summary = "부서 목록 조회", description = "모든 부서의 목록을 조회합니다.")
	@GetMapping(SecurityUtil.LIST_URL)
	public ResponseApi<List<DepartmentDto>> getList() {
		return responseHandler.generateResponse(departmentService.getList());
	}
	
	@Tag(name = "부서 컨트롤러", description = "부서 관련 API")
	@Operation(summary = "부서 등록(ADMIN 전용)", description = "새로운 부서를 등록합니다.")
	@PostMapping(SecurityUtil.INSERT_URL)
	public ResponseApi<DepartmentDto> insert(@RequestBody DepartmentInsertDto requestDto) throws FileTransferException {
		Department department = departmentService.getByName(requestDto.getName());
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(department))) {
			DepartmentDto departmentDto = departmentService.toDepartmentDto(requestDto);
			return responseHandler.generateResponse(departmentService.insert(departmentDto));		
		}
		else throw new FileTransferException(DBErrorCode.DUPLICATE_ENTITY);
	}
	
	@Tag(name = "부서 컨트롤러", description = "부서 관련 API")
	@Operation(summary = "부서 삭제(ADMIN 전용)", description = "지정된 부서를 삭제하고 관련된 데이터를 업데이트합니다.")
	@PostMapping(SecurityUtil.DELETE_URL)
	public ResponseApi<Boolean> delete(@RequestBody DepartmentDeleteDto requestDto) throws FileTransferException {
		if(Boolean.TRUE.equals(requestDto.getDeleteAll())) departmentService.deleteChildrenAll(requestDto.getId());
		else departmentService.updateNewParentId(requestDto.getId(), requestDto.getNewParentId());
		// user department update
		Department department = departmentService.getById(requestDto.getUpdateDepartmentId());
		userService.updateAllDepartment(requestDto.getId(), department);
		// schedule department update
		scheduleService.updateAllDepartment(requestDto.getId(), department);
		
		return responseHandler.generateResponse(departmentService.delete(requestDto.getId()));
	}
	
	@Tag(name = "부서 컨트롤러", description = "부서 관련 API")
	@Operation(summary = "부서 정보 업데이트(ADMIN 전용)", description = "지정된 부서의 정보를 업데이트합니다.")
	@PostMapping(SecurityUtil.UPDATE_URL)
	public ResponseApi<Boolean> update(@RequestBody DepartmentUpdateDto requestDto) throws FileTransferException {
		User manager = userService.getById(requestDto.getManagerId());
		DepartmentDto departmentDto = departmentService.toDepartmentDto(requestDto);
		departmentService.validateUpdate(departmentDto);
		return responseHandler.generateResponse(departmentService.update(departmentDto, manager));
	}
}
