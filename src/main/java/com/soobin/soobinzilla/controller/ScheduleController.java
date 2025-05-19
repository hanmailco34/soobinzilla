package com.soobin.soobinzilla.controller;

import java.util.List;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.dto.response.ListDto;
import com.soobin.soobinzilla.dto.response.ScheduleDto;
import com.soobin.soobinzilla.dto.response.ScheduleListDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.ConnectionService;
import com.soobin.soobinzilla.service.ManagerService;
import com.soobin.soobinzilla.service.ScheduleHistoryService;
import com.soobin.soobinzilla.service.ScheduleService;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "스케줄 컨트롤러", description = "스케줄 관리와 관련된 API 엔드포인트를 제공합니다.")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.SCHEDULE_URL)
@RequiredArgsConstructor
public class ScheduleController {
	
	private final ScheduleService scheduleService;
	
	private final ScheduleHistoryService scheduleHistoryService;
	
	private final ConnectionService connectionService;
	
	private final ManagerService managerService;
	
	private final ResponseHandler responseHandler;
	
	@Tag(name = "스케줄 컨트롤러", description = "스케줄 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "스케줄 리스트(MANGER 이상)", description = "스케줄 리스트를 가져옵니다.")
	@PostMapping(SecurityUtil.LIST_URL)
	public ResponseApi<ListDto<ScheduleListDto>> list(@RequestBody PageDto pageDto) throws FileTransferException {
		Department department = managerService.getDepartment();
		List<Department> childrenDepartment = managerService.getChildrenDepartment();
		if(Boolean.FALSE.equals(ObjectUtil.isEmpty(department))) {
			childrenDepartment.add(department);
		}
		
		return responseHandler.generateResponse(scheduleService.list(pageDto, childrenDepartment));
	}

	@Tag(name = "스케줄 컨트롤러", description = "스케줄 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "스케줄 생성(MANGER 이상)", description = "새로운 스케줄을 생성합니다.")
	@PostMapping(SecurityUtil.INSERT_URL)
	public ResponseApi<ScheduleDto> insert(@RequestBody ScheduleDto requestDto) throws FileTransferException {
		Department department = managerService.getDepartment();
		
		scheduleService.validSchedule(requestDto);
		Connection conection = connectionService.getById(requestDto.getConnectionId());
		return responseHandler.generateResponse(scheduleService.insert(requestDto, conection, department));
	}
	
	@Tag(name = "스케줄 컨트롤러", description = "스케줄 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "스케줄 업데이트(MANGER 이상)", description = "기존 스케줄을 업데이트합니다.")
	@PostMapping(SecurityUtil.UPDATE_URL)
	public ResponseApi<ScheduleDto> update(@RequestBody ScheduleDto requestDto) throws FileTransferException {
		Schedule schedule = scheduleService.getById(requestDto.getId());
		
		managerService.isSubDepartment(schedule.getDepartment());
		
		scheduleService.validSchedule(requestDto);
		return responseHandler.generateResponse(scheduleService.update(requestDto));
	}
	
	@Tag(name = "스케줄 컨트롤러", description = "스케줄 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "스케줄 삭제(MANGER 이상)", description = "지정된 스케줄을 삭제합니다.")
	@PostMapping(SecurityUtil.DELETE_URL)
	public ResponseApi<Boolean> delete(@RequestBody ScheduleDto requestDto) throws FileTransferException {
		Schedule schedule = scheduleService.getById(requestDto.getId());
		
		managerService.isSubDepartment(schedule.getDepartment());
		
		connectionService.delete(requestDto.getConnectionId());
		scheduleHistoryService.delete(schedule);
		scheduleService.delete(schedule);
		return responseHandler.generateResponse(true);
	}
	
	@Tag(name = "스케줄 컨트롤러", description = "스케줄 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "스케줄 활성화 설정(MANGER 이상)", description = "지정된 스케줄의 활성화 여부를 설정합니다.")
	@PostMapping("/setActive")
	public ResponseApi<Boolean> setActive(@RequestBody ScheduleDto requestDto) throws FileTransferException {
		Schedule schedule = scheduleService.getById(requestDto.getId());
		
		managerService.isSubDepartment(schedule.getDepartment());
		
		scheduleService.updateActive(requestDto.getId(), requestDto.getIsActive());
		return responseHandler.generateResponse(true);
	}
}
