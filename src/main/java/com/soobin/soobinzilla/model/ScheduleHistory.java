package com.soobin.soobinzilla.model;

import java.time.LocalDateTime;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.soobin.soobinzilla.model.enums.ScheduleHistoryStatus;
import com.soobin.soobinzilla.util.ConstantUtil;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_schedule_history")
public class ScheduleHistory extends BaseTime {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long	id;
	
	@ManyToOne
	@JoinColumn(name = "schedule_id", nullable = false)
	private Schedule schedule;
	
	@Column(nullable = false)
	private LocalDateTime startTime;
	
	private LocalDateTime endTime;
	
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ScheduleHistoryStatus status;
	
	private String	errorMeesage;
	
	private Long deleteCount;
	
	private Long updateCount;
	
	private Long insertCount;
	
	private Long noDownloadCount;
	
	public void updateStatus(ScheduleHistoryStatus status) {
		this.status = status;
	}
	
	public void updateEndTime(LocalDateTime endTime) {
		this.endTime = endTime;
	}
	
	public void updateErrorMessage(String errorMeesage) {
		this.errorMeesage = errorMeesage;
	}
	
	public void updateCounts(Map<String, Long> counts) {
        this.deleteCount = counts.getOrDefault(ConstantUtil.DELETE_STATUS, 0L);
        this.updateCount = counts.getOrDefault(ConstantUtil.UPDATE_STATUS, 0L);
        this.insertCount = counts.getOrDefault(ConstantUtil.INSERT_STATUS, 0L);
        this.noDownloadCount = counts.getOrDefault(ConstantUtil.NO_DOWNLOAD_STATUS, 0L);
    }
}
