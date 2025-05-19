package com.soobin.soobinzilla.repository.schedule;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import javax.transaction.Transactional;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.QSchedule;
import com.soobin.soobinzilla.model.enums.ScheduleType;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class ScheduleRepositoryImpl implements ScheduleRepositoryCustom {
	
	private final JPAQueryFactory jpaQueryFactory;
	
	public ScheduleRepositoryImpl(JPAQueryFactory jpaQueryFactory) {
		this.jpaQueryFactory = jpaQueryFactory;
	}

	@Override
	@Transactional
	public Long updateAllDepartment(Long departmentId, Department department) {
		QSchedule schedule = QSchedule.schedule;
		return this.jpaQueryFactory.update(schedule)
				.set(schedule.department, department)
				.where(schedule.department.id.eq(departmentId))
				.execute();
	}

	@Override
	public BooleanExpression search(PageDto dto) {
		QSchedule schedule = QSchedule.schedule;
		Map<String, Function<String, BooleanExpression>> searchMap = createSearchMap(schedule);
		
		return Optional.ofNullable(searchMap.get(dto.getSearchBy()))
				.map(predicate -> predicate.apply(dto.getSearch()))
				.orElse(schedule.isNotNull());
	}

	@Override
	public BooleanExpression search(PageDto dto, List<Department> departments) {
		QSchedule schedule = QSchedule.schedule;
		
		BooleanExpression predicate = schedule.department.in(departments);
		
		Map<String, Function<String, BooleanExpression>> searchMap = createSearchMap(schedule);
		
		return Optional.ofNullable(searchMap.get(dto.getSearchBy()))
				.map(searchPredicate -> predicate.and(searchPredicate.apply(dto.getSearch())))
				.orElse(predicate);
	}
	
	private Map<String, Function<String, BooleanExpression>> createSearchMap(QSchedule schedule) {
	    Map<String, Function<String, BooleanExpression>> searchMap = new HashMap<>();
	    searchMap.put("type", search -> {
	    	ScheduleType typeEnum = ScheduleType.valueOf(search.toUpperCase());
	    	return schedule.type.eq(typeEnum);
	    });
	    return searchMap;
	}

}
