package com.soobin.soobinzilla.repository.permission;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.soobin.soobinzilla.model.Permission;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.User;


public interface PermissionRepository extends JpaRepository<Permission, Long>, PermissionRepositoryCustom {
	List<Permission> findAllByUser(User user);
	List<Permission> findAllByUserAndSchedule(User user, Schedule schedule);
}
