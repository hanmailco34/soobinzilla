package com.soobin.soobinzilla.service;

import java.util.List;

import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.response.PermissionDto;
import com.soobin.soobinzilla.mapper.PermissionMapper;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.Permission;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.repository.permission.PermissionRepository;
import com.soobin.soobinzilla.util.ObjectUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class PermissionService {
	
	private final PermissionRepository permissionRepository;
	
	private final PermissionMapper permissionMapper;
	
	public List<PermissionDto> list(Department department) {
		List<Permission> permissionList = Boolean.TRUE.equals(ObjectUtil.isEmpty(department)) ? permissionRepository.findAll() : permissionRepository.findAllByDepartment(department);
		return permissionMapper.toPermissionDtoList(permissionList);
	}

	public List<PermissionDto> insert(List<String> path, User user, Schedule schedule) {
		List<Permission> permissionList = permissionMapper.toPermissionList(user, schedule, path);
		permissionRepository.saveAll(permissionList);
		return permissionMapper.toPermissionDtoList(permissionList);
	}
	
	public List<PermissionDto> get(User user) {
		List<Permission> permissionList = permissionRepository.findAllByUser(user);
		return permissionMapper.toPermissionDtoList(permissionList);
	}
	
	public List<Permission> getPermission(User user) {
		return permissionRepository.findAllByUser(user);
	}
	
	public void deleteAllByUser(User user) {
		List<Permission> exsistPermission = permissionRepository.findAllByUser(user);
		permissionRepository.deleteAllInBatch(exsistPermission);
	}
	
	public void deleteAllByUserAndSchedule(User user, Schedule schedule) {
		List<Permission> exsistPermission = permissionRepository.findAllByUserAndSchedule(user, schedule);
		permissionRepository.deleteAllInBatch(exsistPermission);
	}
	
	/*
	private List<String> toNotExistPathList(List<String> path, User user) {
		List<Permission> exsistPermission = permissionRepository.findAllByUser(user);
		Set<String> exsistPathSet = permissionMapper.toPathSet(exsistPermission);
		
		return path.stream()
				.filter(p -> !exsistPathSet.contains(p))
				.collect(Collectors.toList());
	}
	*/
}
