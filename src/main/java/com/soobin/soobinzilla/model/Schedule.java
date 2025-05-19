package com.soobin.soobinzilla.model;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.soobin.soobinzilla.model.enums.ScheduleType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_schedule")
public class Schedule extends BaseTime {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long	id;
	
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ScheduleType type;
	
	private Integer	hour;
	
	private Integer	minute;
	
	private Integer intervalMinutes;
	
	private Integer dayOfWeek;
	
	private Integer dayOfMonth;
	
	@Column(nullable = false)
	@Builder.Default()
	private Boolean isActive = true;
	
	@OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "connection_id")
    private Connection connection;
	
	@ManyToOne
	@JoinColumn(name = "department_id")
	private Department department;
	
	public void update(ScheduleType type, Integer hour, Integer minute, Integer intervalMinutes, Integer dayOfWeek, Integer dayOfMonth) {
		this.type = type;
		this.hour = hour;
		this.minute = minute;
		this.intervalMinutes = intervalMinutes;
		this.dayOfWeek = dayOfWeek;
		this.dayOfMonth = dayOfMonth;
	}
	
	public void updateActive(Boolean isActive) {
		this.isActive = isActive;
	}
}
