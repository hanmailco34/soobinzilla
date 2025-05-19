package com.soobin.soobinzilla.model;

import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "zilla_department")
public class Department extends BaseTime {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long	id;
	
	@Column(nullable = false, unique = true)
	private String name;
	
	@OneToOne
	@JoinColumn(name = "manager_id")
	private User manager;
	
	@ManyToOne
	@JoinColumn(name = "parent_id")
	private Department parent;
	
	@OneToMany(mappedBy = "parent")
	private List<Department> subDepartments;
	
	@OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
	private List<User> users;
	
    @OneToMany(mappedBy = "department", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Schedule> schedules;
	
	public void updateDepartment(String name, Department parent, User manager) {
		this.name = name;
		this.parent = parent;
		this.manager = manager;
	}
	
}
