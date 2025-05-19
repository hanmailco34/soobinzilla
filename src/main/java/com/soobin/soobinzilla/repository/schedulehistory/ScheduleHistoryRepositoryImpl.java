package com.soobin.soobinzilla.repository.schedulehistory;

import javax.transaction.Transactional;

import org.springframework.stereotype.Repository;

import com.soobin.soobinzilla.model.QScheduleHistory;
import com.soobin.soobinzilla.model.enums.ScheduleHistoryStatus;
import com.querydsl.jpa.impl.JPAQueryFactory;

@Repository
public class ScheduleHistoryRepositoryImpl implements ScheduleHistoryRepositoryCustom {
	
private final JPAQueryFactory queryFactory;
	
	public ScheduleHistoryRepositoryImpl(JPAQueryFactory queryFactory) {
        this.queryFactory = queryFactory;
	}

	@Override
	@Transactional
	public Long initStatus() {
		QScheduleHistory scheduleHistory = QScheduleHistory.scheduleHistory;
		return this.queryFactory.update(scheduleHistory)
				.set(scheduleHistory.status, ScheduleHistoryStatus.COMPLETED)
				.where(scheduleHistory.status.eq(ScheduleHistoryStatus.PROGRESSED))
				.execute();
	}

}
