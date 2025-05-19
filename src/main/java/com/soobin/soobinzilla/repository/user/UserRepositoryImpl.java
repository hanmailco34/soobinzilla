package com.soobin.soobinzilla.repository.user;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import javax.transaction.Transactional;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.QUser;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class UserRepositoryImpl implements UserRepositotyCustom {

	private final JPAQueryFactory queryFactory;
	
	public UserRepositoryImpl(JPAQueryFactory queryFactory) {
        this.queryFactory = queryFactory;
	}
	
	@Override
	@Transactional
	public Long updateAllDepartment(Long departmentId, Department department) {
		QUser user = QUser.user;
		return this.queryFactory.update(user)
			.set(user.department, department)
			.where(user.department.id.eq(departmentId))
			.execute();
	}

	@Override
	public BooleanExpression search(PageDto dto) {
		QUser user = QUser.user;
		Map<String, Function<String, BooleanExpression>> searchMap = createSearchMap(user);
		
		return Optional.ofNullable(searchMap.get(dto.getSearchBy()))
				.map(predicate -> predicate.apply(dto.getSearch()))
				.orElse(user.isNotNull());
	}

	@Override
	public BooleanExpression search(PageDto dto, List<Department> departments) {
		QUser user = QUser.user;
		
		BooleanExpression predicate = user.department.in(departments);
		
		Map<String, Function<String, BooleanExpression>> searchMap = createSearchMap(user);
		
		return Optional.ofNullable(searchMap.get(dto.getSearchBy()))
				.map(searchPredicate -> predicate.and(searchPredicate.apply(dto.getSearch())))
				.orElse(predicate);
	}
	
	private Map<String, Function<String, BooleanExpression>> createSearchMap(QUser user) {
	    Map<String, Function<String, BooleanExpression>> searchMap = new HashMap<>();
	    searchMap.put("name", search -> user.name.like("%" + search + "%"));
	    searchMap.put("userId", search -> user.userId.like("%" + search + "%"));
	    return searchMap;
	}
}
