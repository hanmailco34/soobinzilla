package com.soobin.soobinzilla.service;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.dto.response.ListDto;
import com.soobin.soobinzilla.dto.response.ScheduleDto;
import com.soobin.soobinzilla.dto.response.ScheduleListDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;
import com.soobin.soobinzilla.mapper.ScheduleMapper;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.enums.ScheduleType;
import com.soobin.soobinzilla.repository.schedule.ScheduleRepository;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.PageUtil;
import com.soobin.soobinzilla.util.TimeUtil;
import com.soobin.soobinzilla.util.ValidUtil;
import com.querydsl.core.types.Predicate;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ScheduleService {

	private final ScheduleRepository scheduleRepository;
	
	private final ScheduleMapper scheduleMapper;
	
	public void validSchedule(ScheduleDto requestDto) throws FileTransferException {
		ValidUtil.checkNull(requestDto.getType());
		
		if(ScheduleType.INTERVAL.equals(requestDto.getType())) {
			validateIntervalSchedule(requestDto);
		} else {
			validateHourAndMinuteSchedule(requestDto);
			
			if(ScheduleType.WEEKLY.equals(requestDto.getType())) {
				if(Boolean.FALSE.equals(TimeUtil.validDayOfWeek(requestDto.getDayOfWeek()))) throw new FileTransferException(DBErrorCode.NOT_PATTERN);
			} else if(ScheduleType.MONTHLY.equals(requestDto.getType()) && Boolean.FALSE.equals(TimeUtil.validDayOfMonth(requestDto.getDayOfMonth()))) throw new FileTransferException(DBErrorCode.NOT_PATTERN);
		}
	}
	
	private void validateIntervalSchedule(ScheduleDto requestDto) throws FileTransferException {
	    Integer intervalMinutes = requestDto.getIntervalMinutes();
	    ValidUtil.checkNull(intervalMinutes);
	    if (intervalMinutes <= 0) {
	        throw new FileTransferException(DBErrorCode.NOT_PATTERN);
	    }
	}
	
	private void validateHourAndMinuteSchedule(ScheduleDto requestDto) throws FileTransferException {
	    Integer hour = requestDto.getHour();
	    Integer minute = requestDto.getMinute();
	    if(Boolean.FALSE.equals((TimeUtil.validHour(hour) && TimeUtil.validMinute(minute)))) {
	    	throw new FileTransferException(DBErrorCode.NOT_PATTERN);
	    }
	}
	
	public Schedule getById(Long id) throws FileTransferException {
		ValidUtil.checkNull(id);
		Optional<Schedule> schedule = scheduleRepository.findById(id);
		return schedule.orElseThrow(() -> new FileTransferException(DBErrorCode.NOT_FOUND));
	}
	
	public ScheduleDto insert(ScheduleDto requestDto, Connection connection, Department department) {
		Schedule schedule = scheduleMapper.toShedule(requestDto, connection, department);
		scheduleRepository.save(schedule);
		return scheduleMapper.toSheduleDto(schedule);
	}
	
	public ScheduleDto update(ScheduleDto requestDto) throws FileTransferException {
		Schedule schedule = getById(requestDto.getId());
		schedule.update(requestDto.getType(), requestDto.getHour(), requestDto.getMinute(), requestDto.getIntervalMinutes(), requestDto.getDayOfWeek(), requestDto.getDayOfMonth());
		scheduleRepository.save(schedule);
		return scheduleMapper.toSheduleDto(schedule);
	}
	
	public void updateActive(Long id, Boolean isActive) throws FileTransferException {
		ValidUtil.checkNull(isActive);
		Schedule schedule = getById(id);
		schedule.updateActive(isActive);
		scheduleRepository.save(schedule);
	}
	
	public Long updateAllDepartment(Long departmentId, Department department) {
		return scheduleRepository.updateAllDepartment(departmentId, department);
	}
	
	public void delete(Schedule schedule) {
		scheduleRepository.delete(schedule);
	}
	
	public List<Schedule> getActiveList() { 
		return scheduleRepository.findByIsActive(true);
	}
	
	public Boolean checkExecution(Schedule schedule, LocalDateTime lastExecutionTime, LocalDateTime now) {
		
		if(ScheduleType.INTERVAL.equals(schedule.getType())) {
			return ChronoUnit.MINUTES.between(lastExecutionTime, now) >= schedule.getIntervalMinutes();
		} else if(ScheduleType.DAILY.equals(schedule.getType())) {
			return now.getHour() == schedule.getHour()
					&& now.getMinute() == schedule.getMinute()
					&& Boolean.FALSE.equals(TimeUtil.isSameDay(lastExecutionTime, now));
		} else if(ScheduleType.WEEKLY.equals(schedule.getType())) {
			return now.getHour() == schedule.getHour()
					&& now.getMinute() == schedule.getMinute()
					&& now.getDayOfWeek().getValue() == schedule.getDayOfWeek()
					&& Boolean.FALSE.equals(TimeUtil.isSameWeek(lastExecutionTime, now));
		} else if(ScheduleType.MONTHLY.equals(schedule.getType())) {
			return now.getHour() == schedule.getHour()
					&& now.getMinute() == schedule.getMinute()
					&& now.getDayOfMonth() == schedule.getDayOfMonth()
					&& Boolean.FALSE.equals(TimeUtil.isSameMonth(lastExecutionTime, now));
		}
		return false;
	}
	
	public ListDto<ScheduleListDto> list(PageDto requestDto, List<Department> departments) throws FileTransferException {
		Pageable pageable = PageUtil.getPageable(requestDto);
		Predicate predicate = Boolean.TRUE.equals(ObjectUtil.isEmpty(departments)) ? scheduleRepository.search(requestDto) : scheduleRepository.search(requestDto, departments);
		Long total = scheduleRepository.count(predicate);
		Page<Schedule> schedules = scheduleRepository.findAll(predicate, pageable);
		List<ScheduleListDto> result = schedules.stream()
								.map(scheduleMapper::toSheduleListDto)
								.collect(Collectors.toList());
		
		return ListDto.<ScheduleListDto>builder()
				.content(result)
				.total(total)
				.build();
	}
}
