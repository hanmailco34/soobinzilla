package com.soobin.soobinzilla.mapper;

import java.time.LocalDateTime;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.ScheduleHistory;
import com.soobin.soobinzilla.model.enums.ScheduleHistoryStatus;

@Component
public class ScheduleHistoryMapper {

	public ScheduleHistory toScheduleHistory(Schedule schedule, LocalDateTime now) {
		return ScheduleHistory.builder()
				.schedule(schedule)
				.startTime(now)
				.status(ScheduleHistoryStatus.STARTED)
				.build();
	}
}
