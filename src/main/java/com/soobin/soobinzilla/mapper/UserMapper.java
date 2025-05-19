package com.soobin.soobinzilla.mapper;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.dto.request.UserInsertDto;
import com.soobin.soobinzilla.dto.request.UserUpdateDto;
import com.soobin.soobinzilla.dto.request.UsersInsertDto;
import com.soobin.soobinzilla.dto.request.UsersUpdateDto;
import com.soobin.soobinzilla.dto.response.UserDto;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.CryptoUtil;
import com.soobin.soobinzilla.util.ObjectUtil;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class UserMapper {
	
	private final CryptoUtil cryptoUtil;
	
	public User updateUser(User user, UserDto requestDto, String type) {
		if(ConstantUtil.INFO_UPDATE.equals(type)) {
			user.updateName(requestDto.getName());
		} else if(ConstantUtil.PASSWORD_UPDATE.equals(type)) {
			user.updatePassword(cryptoUtil.sha512(requestDto.getPassword()));
		} else if(ConstantUtil.ROLE_UPDATE.equals(type)) {
			user.updateRole(requestDto.getRole());
		} 
		return user;
	}

	public User toUser(UserDto requestDto, Department department) {
		return User.builder()
				.userId(requestDto.getUserId())
				.password(cryptoUtil.sha512(requestDto.getPassword()))
				.name(requestDto.getName())
				.role(requestDto.getRole())
				.department(department)
				.build();
	}
	
	public UserDto toUserDto(User user) {
		Long departmendId = (Boolean.TRUE.equals(ObjectUtil.isEmpty(user.getDepartment()))) ?  null : user.getDepartment().getId();
		
		return UserDto.builder()
				.id(user.getId())
				.userId(user.getUserId())
				.name(user.getName())
				.role(user.getRole())
				.createAt(user.getCreateAt())
				.departmentId(departmendId)
				.build();
	}
	
	public UserDto toUserDto(UserInsertDto requestDto) {
		return UserDto.builder()
				.userId(requestDto.getUserId())
				.password(cryptoUtil.sha512(requestDto.getPassword()))
				.name(requestDto.getName())
				.build();
	}
	
	public UserDto toUserDto(UserUpdateDto requestDto) {
		return UserDto.builder()
				.password(cryptoUtil.sha512(requestDto.getPassword()))
				.name(requestDto.getName())
				.build();
	}
	
	public UserDto toUserDto(UsersInsertDto requestDto) {
		return UserDto.builder()
				.userId(requestDto.getUserId())
				.password(cryptoUtil.sha512(requestDto.getPassword()))
				.name(requestDto.getName())
				.role(requestDto.getRole())
				.build();
	}
	
	public UserDto toUserDto(UsersUpdateDto requestDto) {
		return UserDto.builder()
				.password(cryptoUtil.sha512(requestDto.getPassword()))
				.name(requestDto.getName())
				.role(requestDto.getRole())
				.build();
	}
}
