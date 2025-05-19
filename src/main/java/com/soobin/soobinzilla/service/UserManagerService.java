package com.soobin.soobinzilla.service;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.request.PageDto;
import com.soobin.soobinzilla.dto.request.UsersInsertDto;
import com.soobin.soobinzilla.dto.request.UsersUpdateDto;
import com.soobin.soobinzilla.dto.response.ListDto;
import com.soobin.soobinzilla.dto.response.UserDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.mapper.UserMapper;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.repository.user.UserRepository;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.PageUtil;
import com.querydsl.core.types.Predicate;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class UserManagerService {
	
	private final UserRepository userRepository;
	
	private final UserMapper userMapper;
	
	public ListDto<UserDto> list(PageDto requestDto, List<Department> departments) throws FileTransferException {
		Pageable pageable = PageUtil.getPageable(requestDto);
		Predicate predicate = Boolean.TRUE.equals(ObjectUtil.isEmpty(departments)) ? userRepository.search(requestDto) : userRepository.search(requestDto, departments);
		Long total = userRepository.count(predicate);
		Page<User> users = userRepository.findAll(predicate, pageable);
		List<UserDto> result = users.stream()
								.map(userMapper::toUserDto)
								.collect(Collectors.toList());
		
		return ListDto.<UserDto>builder()
				.content(result)
				.total(total)
				.build();
	}
	
	public UserDto toUserDto(UsersInsertDto requestDto) {
		return userMapper.toUserDto(requestDto);
	}
	
	public UserDto toUserDto(UsersUpdateDto requestDto) {
		return userMapper.toUserDto(requestDto);
	}
	
	public UserDto insert(UserDto userDto, Department department) {
		User user = userMapper.toUser(userDto, department);
		return userSave(user);
	}
	
	public UserDto update(User user, UserDto userDto, Department department, String type) {
		User updateUser = userMapper.updateUser(user, userDto, type);
		updateUser.updateDepartment(department);
		return userSave(updateUser);
	}
	
	private UserDto userSave(User user) {
		userRepository.save(user);
		return userMapper.toUserDto(user);
	}
}
