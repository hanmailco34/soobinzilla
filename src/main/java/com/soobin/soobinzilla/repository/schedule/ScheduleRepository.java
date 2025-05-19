package com.soobin.soobinzilla.repository.schedule;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;

import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Schedule;

public interface ScheduleRepository extends JpaRepository<Schedule, Long>, ScheduleRepositoryCustom, QuerydslPredicateExecutor<Schedule> {
	List<Schedule> findByIsActive(boolean isActive);
	List<Schedule> findAllByDepartment(Department department);
}
