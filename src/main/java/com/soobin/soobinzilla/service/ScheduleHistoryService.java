package com.soobin.soobinzilla.service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.mapper.ScheduleHistoryMapper;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.ScheduleHistory;
import com.soobin.soobinzilla.model.enums.ScheduleHistoryStatus;
import com.soobin.soobinzilla.repository.schedulehistory.ScheduleHistoryRepository;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ScheduleHistoryService {
	
	private final ScheduleHistoryRepository scheduleHistoryRepository;
	
	private final ScheduleHistoryMapper scheduleHistoryMapper;
	
	public boolean isProgressSchedule(Schedule schedule) {
		Optional<ScheduleHistory> progressSchedule = scheduleHistoryRepository.findTopByScheduleAndStatusOrderByStartTimeDesc(schedule, ScheduleHistoryStatus.PROGRESSED);
		return progressSchedule.isPresent();
	}
	
	public ScheduleHistory getLastExecution(Schedule schedule) {
		Optional<ScheduleHistory> lastExecution = scheduleHistoryRepository.findTopByScheduleAndStatusOrderByEndTimeDesc(schedule, ScheduleHistoryStatus.COMPLETED);
		return lastExecution.orElse(null);
	}
	
	public ScheduleHistory insert(Schedule schedule, LocalDateTime now) {
		ScheduleHistory scheduleHistory = scheduleHistoryMapper.toScheduleHistory(schedule, now);
		scheduleHistoryRepository.save(scheduleHistory);
		return scheduleHistory;
	}
	
	public void delete(Schedule schedule) {
		List<ScheduleHistory> list = scheduleHistoryRepository.findBySchedule(schedule);
		scheduleHistoryRepository.deleteAllInBatch(list);
	}
	
	public void updateProgress(ScheduleHistory scheduleHistory) {
		scheduleHistory.updateStatus(ScheduleHistoryStatus.PROGRESSED);
		scheduleHistoryRepository.save(scheduleHistory);
	}
	
	public void updateComplete(ScheduleHistory scheduleHistory, Map<String, Long> downloadCount) {
		scheduleHistory.updateEndTime(LocalDateTime.now());
		scheduleHistory.updateStatus(ScheduleHistoryStatus.COMPLETED);
		scheduleHistory.updateCounts(downloadCount);
		scheduleHistoryRepository.save(scheduleHistory);
	}
	
	public void updateFail(ScheduleHistory scheduleHistory, String errorMessage) {
		scheduleHistory.updateEndTime(LocalDateTime.now());
		scheduleHistory.updateStatus(ScheduleHistoryStatus.FAILED);
		scheduleHistory.updateErrorMessage(errorMessage);
		scheduleHistoryRepository.save(scheduleHistory);
	}
	
	public void initStatus() {
		scheduleHistoryRepository.initStatus();
	}
}
