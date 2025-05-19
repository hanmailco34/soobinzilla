package com.soobin.soobinzilla.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.request.PermissionInsertDto;
import com.soobin.soobinzilla.dto.response.PermissionDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.filetransfer.vo.FileInfoVO;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Permission;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.ConnectionService;
import com.soobin.soobinzilla.service.FileTransferService;
import com.soobin.soobinzilla.service.ManagerService;
import com.soobin.soobinzilla.service.PermissionService;
import com.soobin.soobinzilla.service.ScheduleService;
import com.soobin.soobinzilla.service.UserService;
import com.soobin.soobinzilla.util.AuthUtil;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "권한 컨트롤러", description = "권한 관리와 관련된 API 엔드포인트를 제공합니다.")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.PERMISSION_URL)
@RequiredArgsConstructor
public class PermissionController {
	
	private final UserService userService;
	
	private final ScheduleService scheduleService;
	
	private final PermissionService permissionService;
	
	private final FileTransferService fileTransferService;
	
	private final ConnectionService connectionService;
	
	private final ManagerService managerService;
	
	private final AuthUtil authUtil;
	
	private final ResponseHandler responseHandler;
	
	@Tag(name = "권한 컨트롤러", description = "권한 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "권한 목록 조회(MANAGER 이상)", description = "관리자의 부서 및 하위 부서의 권한 목록을 조회합니다.")
	@GetMapping(SecurityUtil.LIST_URL)
	public ResponseApi<List<PermissionDto>> list() throws FileTransferException {
		Department department = managerService.getDepartment();
		List<PermissionDto> result = permissionService.list(department);
		
		List<Department> childrenDepartment = managerService.getChildrenDepartment();
		for(Department children : childrenDepartment) {
			result.addAll(permissionService.list(children));
		}
		
		return responseHandler.generateResponse(result);
	}
	
	@Tag(name = "권한 컨트롤러", description = "권한 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "사용자 권한 조회", description = "현재 로그인한 사용자의 권한을 조회합니다.")
	@GetMapping("")
	public ResponseApi<List<PermissionDto>> get() throws FileTransferException {
		Long id = authUtil.getPayload().getId();
		User user = userService.getById(id);
		
		return responseHandler.generateResponse(permissionService.get(user));
	}
	
	@Tag(name = "권한 컨트롤러", description = "권한 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "디렉토리 내용 조회", description = "로그인한 사용자의 권한에 따라 업로드한 디렉토리의 내용을 읽어옵니다.")
	@GetMapping(SecurityUtil.READ_DIRECTORY_URL)
	public ResponseApi<List<Map<String, List<FileInfoVO>>>> readDirectory() throws FileTransferException {
		Long id = authUtil.getPayload().getId();
		User user = userService.getById(id);
		
		List<Permission> permissionList = permissionService.getPermission(user);
		List<Map<String, List<FileInfoVO>>> result = new ArrayList<>();
		for(Permission permission : permissionList) {
			List<Connection> targetList = connectionService.findByIsDownload(false);
			for(Connection target : targetList) {
				Map<String, List<FileInfoVO>> read = fileTransferService.readPermissionDirectory(target, permission.getSchedule().getConnection().getId(), permission.getPath());
				if(Boolean.FALSE.equals(ObjectUtil.isEmpty(read))) result.add(read);
			}
		}
		
		return responseHandler.generateResponse(result);
	}
	
	@Tag(name = "권한 컨트롤러", description = "권한 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "권한 추가(MANAGER 이상)", description = "특정 사용자에게 권한을 추가합니다.")
	@PostMapping(SecurityUtil.INSERT_URL)
	public ResponseApi<List<PermissionDto>> insert(@RequestBody PermissionInsertDto permissionInsertDto) throws FileTransferException {		
		User user = userService.getById(permissionInsertDto.getUserId());
		Schedule schedule = scheduleService.getById(permissionInsertDto.getScheduleId());
		Department department = user.getDepartment();
		
		managerService.isSubDepartment(department);
		managerService.isSubDepartment(schedule.getDepartment());
		
		permissionService.deleteAllByUserAndSchedule(user, schedule);
		
		return responseHandler.generateResponse(permissionService.insert(permissionInsertDto.getPath(), user, schedule));
	}

}
