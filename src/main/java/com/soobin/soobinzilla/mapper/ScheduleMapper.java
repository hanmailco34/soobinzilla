package com.soobin.soobinzilla.mapper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.dto.response.ScheduleDto;
import com.soobin.soobinzilla.dto.response.ScheduleListDto;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Schedule;

@Component
public class ScheduleMapper {
	
	@Autowired
	private ConnectionMapper connectionMapper;

	public Schedule toShedule(ScheduleDto scheduleDto, Connection connection, Department department) {
		return Schedule.builder()
				.type(scheduleDto.getType())
				.intervalMinutes(scheduleDto.getIntervalMinutes())
				.hour(scheduleDto.getHour())
				.minute(scheduleDto.getMinute())
				.dayOfWeek(scheduleDto.getDayOfWeek())
				.dayOfMonth(scheduleDto.getDayOfMonth())
				.connection(connection)
				.department(department)
				.build();
	}
	
	public ScheduleDto toSheduleDto(Schedule shcedule) {
		return ScheduleDto.builder()
				.id(shcedule.getId())
				.type(shcedule.getType())
				.intervalMinutes(shcedule.getIntervalMinutes())
				.hour(shcedule.getHour())
				.minute(shcedule.getMinute())
				.dayOfWeek(shcedule.getDayOfWeek())
				.dayOfMonth(shcedule.getDayOfMonth())
				.isActive(shcedule.getIsActive())
				.build();
	}
	
	public ScheduleListDto toSheduleListDto(Schedule shcedule) {
		return ScheduleListDto.builder()
				.id(shcedule.getId())
				.type(shcedule.getType())
				.intervalMinutes(shcedule.getIntervalMinutes())
				.hour(shcedule.getHour())
				.minute(shcedule.getMinute())
				.dayOfWeek(shcedule.getDayOfWeek())
				.dayOfMonth(shcedule.getDayOfMonth())
				.isActive(shcedule.getIsActive())
				.connectionDto(connectionMapper.toConnectionDto(shcedule.getConnection()))
				.build();
	}
}
