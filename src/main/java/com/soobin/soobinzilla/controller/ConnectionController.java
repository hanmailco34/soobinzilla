package com.soobin.soobinzilla.controller;

import java.util.Date;
import java.util.List;

import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.soobin.soobinzilla.dto.response.ConnectionDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.response.ResponseApi;
import com.soobin.soobinzilla.response.ResponseHandler;
import com.soobin.soobinzilla.service.ConnectionService;
import com.soobin.soobinzilla.service.ManagerService;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.SecurityUtil;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

@Tag(name = "연결 컨트롤러", description = "연결 관리와 관련된 API 엔드포인트를 제공합니다.")
@RestController
@RequestMapping("${app.api.prefix}" + SecurityUtil.CONNECTION_URL)
@RequiredArgsConstructor
public class ConnectionController {
	
	private final ConnectionService connectionService;
	
	private final ManagerService managerService;
	
	private final ResponseHandler responseHandler;
	
	@Tag(name = "연결 컨트롤러", description = "연결 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "연결 생성(MANGER 이상)", description = "새로운 연결을 생성합니다.")
	@PostMapping(SecurityUtil.INSERT_URL)
	public ResponseApi<ConnectionDto> insert(@RequestBody ConnectionDto connectionDto) throws FileTransferException {
		return responseHandler.generateResponse(connectionService.insert(connectionDto));
	}
	
	@Tag(name = "연결 컨트롤러", description = "연결 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "연결 업데이트(MANGER 이상)", description = "기존 연결을 업데이트합니다.")
	@PostMapping(SecurityUtil.UPDATE_URL)
	public ResponseApi<ConnectionDto> update(@RequestBody ConnectionDto connectionDto) throws FileTransferException {
		Connection connection = connectionService.getById(connectionDto.getId());
		
		managerService.isSubDepartment(connection.getSchedule().getDepartment());
		
		return responseHandler.generateResponse(connectionService.update(connectionDto));
	}
	
	@Tag(name = "연결 컨트롤러", description = "연결 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "연결 삭제(MANGER 이상)", description = "지정된 연결을 삭제합니다. 스케쥴이 있는 경우 삭제되지 않습니다.")
	@PostMapping(SecurityUtil.DELETE_URL)
	public ResponseApi<Boolean> delete(@RequestBody ConnectionDto connectionDto) throws FileTransferException {
		connectionService.delete(connectionDto.getId());
		return responseHandler.generateResponse(true);
	}
	
	@Tag(name = "연결 컨트롤러", description = "연결 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "부서별 로그 조회(MANGER 이상)", description = "지정된 ID로 부서별 로그를 조회합니다.")
	@GetMapping("/getLog/{id}")
	public ResponseApi<List<String>> getLog(@PathVariable Long id) throws FileTransferException {
		return responseHandler.generateResponse(connectionService.getLog(id));
	}
	
	@Tag(name = "연결 컨트롤러", description = "연결 관리와 관련된 API 엔드포인트를 제공합니다.")
	@Operation(summary = "부서별 로그 날짜별 조회(MANGER 이상)", description = "지정된 ID와 날짜(format: yyyy-MM-dd)로 부서별 로그를 조회합니다.")
	@GetMapping("/getLog/{id}/{date}")
	public ResponseApi<List<String>> getLog(@PathVariable Long id, @PathVariable @DateTimeFormat(pattern = ConstantUtil.CONNECTION_DATE_LOG_PATTERN) Date date) throws FileTransferException {
		return responseHandler.generateResponse(connectionService.getLog(id, date));
	}
}
