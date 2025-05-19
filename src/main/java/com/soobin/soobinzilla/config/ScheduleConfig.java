package com.soobin.soobinzilla.config;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.ScheduleHistory;
import com.soobin.soobinzilla.service.ConnectionService;
import com.soobin.soobinzilla.service.FileTransferService;
import com.soobin.soobinzilla.service.ScheduleHistoryService;
import com.soobin.soobinzilla.service.ScheduleService;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.LogUtil;
import com.soobin.soobinzilla.util.ObjectUtil;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class ScheduleConfig {
	
	private final ScheduleService scheduleService;
	
	private final ScheduleHistoryService scheduleHistoryService;
	
	private final FileTransferService fileTransferService;
	
	private final ConnectionService connectionService;

	@Scheduled(fixedRate = 10000)
	public void scheduleRunner() {
		LocalDateTime now = LocalDateTime.now();
		List<Schedule> activeScheduleList = scheduleService.getActiveList();
		
		for(Schedule schdule : activeScheduleList) {
			if(Boolean.TRUE.equals(isExecutable(schdule, now))) {
				
				ScheduleHistory scheduleHistory = scheduleHistoryService.insert(schdule, now);
				scheduleHistoryService.updateProgress(scheduleHistory);
				
				try {
					Map<String, Long> downloadCount = fileTransferService.download(schdule.getConnection());
					if(Boolean.TRUE.equals(isUploadStatus(downloadCount))) {
						List<Connection> targetList = connectionService.findByIsDownload(false);
						for(Connection target : targetList) {
							String localDirectory = schdule.getConnection().getConnectionConfig().getLocalDirectory();
							String serverDirectory = target.getConnectionConfig().getServerDirectory() + ConstantUtil.FILE_SEPARATOR + schdule.getConnection().getId();
							target.getConnectionConfig().updateDirectory(localDirectory, serverDirectory);
							fileTransferService.upload(target);
							fileTransferService.deleteWithSuffix(target);
						}
						fileTransferService.resetIndexFile(schdule.getConnection());
					}
					scheduleHistoryService.updateComplete(scheduleHistory, downloadCount);
				} catch(FileTransferException e) {
					String errorMessage = e.getError().getDescription() + e.getMessage();
					scheduleHistoryService.updateFail(scheduleHistory, errorMessage);
					LogUtil.connection(schdule.getConnection().getId(), errorMessage);
				}
			}
		}
	}
	
	private Boolean isExecutable(Schedule schedule, LocalDateTime now) {
		if(Boolean.TRUE.equals(scheduleHistoryService.isProgressSchedule(schedule))) return false;
		
		ScheduleHistory lastExecution = scheduleHistoryService.getLastExecution(schedule);
		
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(lastExecution))) return true;
		
		LocalDateTime lastExecutionTime = lastExecution.getEndTime();
		
		return scheduleService.checkExecution(schedule, lastExecutionTime, now);
	}
	
	private Boolean isUploadStatus(Map<String, Long> downloadCount) {
		return downloadCount.containsKey(ConstantUtil.INSERT_STATUS) || downloadCount.containsKey(ConstantUtil.UPDATE_STATUS) || downloadCount.containsKey(ConstantUtil.DELETE_STATUS);
	}
}
