package com.soobin.soobinzilla.repository.permission;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Permission;
import com.soobin.soobinzilla.model.QPermission;
import com.soobin.soobinzilla.model.QSchedule;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class PermissionRepositoryImpl implements PermissionRepositoryCustom {

	private final JPAQueryFactory jpaQueryFactory;
	
	public PermissionRepositoryImpl(JPAQueryFactory jpaQueryFactory) {
		this.jpaQueryFactory = jpaQueryFactory;
	}

	@Override
	public List<Permission> findAllByDepartment(Department department) {
		QPermission permission = QPermission.permission;
		QSchedule schedule = QSchedule.schedule;
		
		return this.jpaQueryFactory.select(permission)
				.from(permission)
				.join(schedule).on(schedule.eq(permission.schedule))
				.where(schedule.department.eq(department))
				.fetch();
	}
}
