package com.soobin.soobinzilla.mapper;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.dto.response.PermissionDto;
import com.soobin.soobinzilla.model.Permission;
import com.soobin.soobinzilla.model.Schedule;
import com.soobin.soobinzilla.model.User;

@Component
public class PermissionMapper {
	
	public Set<String> toPathSet(List<Permission> permissionList) {
		return permissionList.stream()
				.map(Permission::getPath)
				.collect(Collectors.toSet());
	}

	public List<Permission> toPermissionList(User user, Schedule schedule, List<String> path) {
		return path.stream()
			.map(e -> Permission.builder()
						.user(user)
						.schedule(schedule)
						.path(e)
						.build()
			).collect(Collectors.toList());
	}
	
	public List<PermissionDto> toPermissionDtoList(List<Permission> permissionList) {
		return permissionList.stream()
				.map(e -> PermissionDto.builder()
							.id(e.getId())
							.userId(e.getUser().getId())
							.scheduleId(e.getSchedule().getId())
							.path(e.getPath())
							.build()
				).collect(Collectors.toList());
	}
}
