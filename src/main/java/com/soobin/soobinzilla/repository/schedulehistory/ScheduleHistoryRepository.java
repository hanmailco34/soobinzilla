package com.soobin.soobinzilla.repository.schedulehistory;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.ScheduleHistory;
import com.soobin.soobinzilla.model.enums.ScheduleHistoryStatus;
import java.util.List;


public interface ScheduleHistoryRepository extends JpaRepository<ScheduleHistory, Long>, ScheduleHistoryRepositoryCustom {
	List<ScheduleHistory> findBySchedule(Schedule schedule);
	Optional<ScheduleHistory> findTopByScheduleAndStatusOrderByStartTimeDesc(Schedule schedule, ScheduleHistoryStatus status);
	Optional<ScheduleHistory> findTopByScheduleAndStatusOrderByEndTimeDesc(Schedule schedule, ScheduleHistoryStatus status);
}
