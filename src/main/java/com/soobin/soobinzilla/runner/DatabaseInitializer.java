package com.soobin.soobinzilla.runner;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.model.enums.ProtocolType;
import com.soobin.soobinzilla.service.ConnectionService;
import com.soobin.soobinzilla.service.ScheduleHistoryService;
import com.soobin.soobinzilla.service.UserService;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class DatabaseInitializer implements ApplicationRunner {
	
	private final UserService userService;
	
	private final ConnectionService connectionService;
	
	private final ScheduleHistoryService scheduleHistoryService;
	
	@Value("${app.admin.id}")
	private String adminId;
	
	@Value("${app.admin.password}")
	private String password;
	
	@Value("${app.target.ip}")
	private String ip;
	
	@Value("${app.target.port}")
	private Integer port;
	
	@Value("${app.target.username}")
	private String username;
	
	@Value("${app.target.password}")
	private String pwd;
	
	@Value("${app.target.server-directory}")
	private String serverDirectory;
	
	@Value("${app.target.protocol}")
	private ProtocolType protocol;

	@Override
	public void run(ApplicationArguments args) throws Exception {
		userService.initAdmin(adminId, password);
		connectionService.initConnection(ip, port, username, pwd, serverDirectory, protocol);
		scheduleHistoryService.initStatus();
	}
	
}
