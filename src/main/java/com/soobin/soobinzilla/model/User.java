package com.soobin.soobinzilla.model;

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

import com.soobin.soobinzilla.model.enums.Role;
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
@Table(name = "zilla_user")
public class User extends BaseTime {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long	id;
	
	@Column(nullable = false, unique = true, length = ConstantUtil.USER_ID_LENGTH)
	private String	userId;
	
	@Column(nullable = false, length = ConstantUtil.PASSWORD_LENGTH)
	private String	password;
	
	@Column(nullable = false, length = ConstantUtil.NAME_LENGTH)
	private String	name;
	
	@Column(nullable = false)
	@Builder.Default()
	private Boolean isActive = true;
	
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private Role	role;
	
	@ManyToOne
	@JoinColumn(name = "department_id", referencedColumnName = "id")
	private Department department;
	
	public void updateActive(Boolean isActive) {
		this.isActive = isActive;
	}
	
	public void updateName(String name) {
		this.name = name;
	}
	
	public void updatePassword(String password) {
		this.password = password;
	}
	
	public void updateRole(Role role) {
		this.role = role;
	}
	
	public void updateDepartment(Department department) {
		this.department = department;
	}
}
