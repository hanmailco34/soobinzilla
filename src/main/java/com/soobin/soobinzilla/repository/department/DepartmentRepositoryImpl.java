package com.soobin.soobinzilla.repository.department;

import javax.transaction.Transactional;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.model.QDepartment;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class DepartmentRepositoryImpl implements DepartmentRepositoryCustom {
	
	private final JPAQueryFactory queryFactory;
	
	public DepartmentRepositoryImpl(JPAQueryFactory queryFactory) {
        this.queryFactory = queryFactory;
	}

	@Override
	@Transactional
	public Long deleteChildrenByParentId(Long parentId) {
		QDepartment department = QDepartment.department;
		return this.queryFactory.delete(department)
								.where(department.parent.id.eq(parentId))
								.execute();
	}

	@Override
	@Transactional
	public Long updateChildrenNewParentId(Long parentId, Long newParentId) {
		QDepartment department = QDepartment.department;
		return this.queryFactory.update(department)
				.set(department.parent.id, newParentId)
				.where(department.parent.id.eq(parentId))
				.execute();
	}
}
