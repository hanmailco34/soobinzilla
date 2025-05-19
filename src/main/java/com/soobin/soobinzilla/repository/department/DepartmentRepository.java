package com.soobin.soobinzilla.repository.department;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.soobin.soobinzilla.model.Department;

public interface DepartmentRepository extends JpaRepository<Department, Long>, DepartmentRepositoryCustom {
	Optional<Department> findByName(String name);
	
	Optional<Department> findByManagerId(Long managerId);
}
