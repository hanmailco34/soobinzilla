package com.soobin.soobinzilla.model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_path", uniqueConstraints = @UniqueConstraint(columnNames = {"user_id", "schedule_id", "path"}))
public class Permission extends BaseTime {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	
	@ManyToOne
	@JoinColumn(name = "user_id", referencedColumnName = "id", nullable = false)
	private User user;
	
	@ManyToOne
	@JoinColumn(name = "schedule_id", referencedColumnName = "id", nullable = false)
	private Schedule schedule;
	
	private String path; 
}
