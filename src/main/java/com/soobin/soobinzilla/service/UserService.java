package com.soobin.soobinzilla.service;

import java.util.Optional;

import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.request.UserInsertDto;
import com.soobin.soobinzilla.dto.request.UserUpdateDto;
import com.soobin.soobinzilla.dto.response.UserDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;
import com.soobin.soobinzilla.mapper.UserMapper;
import com.soobin.soobinzilla.model.Department;
import com.soobin.soobinzilla.model.User;
import com.soobin.soobinzilla.model.enums.Role;
import com.soobin.soobinzilla.repository.user.UserRepository;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.ValidUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class UserService {

	private final UserRepository userRepository;
	
	private final UserMapper userMapper;
	
	public void validName(String name) throws FileTransferException {
		ValidUtil.checkNull(name);
		if(Boolean.TRUE.equals(ValidUtil.isLengthOver(name, ConstantUtil.NAME_LENGTH))) throw new FileTransferException(DBErrorCode.LENGTH_OVER);
	}
	
	public void validPassword(String password) throws FileTransferException {
		ValidUtil.checkNull(password);
		if(Boolean.TRUE.equals(ValidUtil.isLengthOver(password, ConstantUtil.PASSWORD_LENGTH))) throw new FileTransferException(DBErrorCode.LENGTH_OVER);
		if(Boolean.FALSE.equals(ValidUtil.isPattern(password, ConstantUtil.PASSWORD_PATTERN))) throw new FileTransferException(DBErrorCode.NOT_PATTERN);
	}
	
	public void validUserId(String userId) throws FileTransferException {
		ValidUtil.checkNull(userId);
		if(Boolean.TRUE.equals(ValidUtil.isLengthOver(userId, ConstantUtil.USER_ID_LENGTH))) throw new FileTransferException(DBErrorCode.LENGTH_OVER);
		if(Boolean.FALSE.equals(ValidUtil.isPattern(userId, ConstantUtil.USER_ID_PATTERN))) throw new FileTransferException(DBErrorCode.NOT_PATTERN);
		if(Boolean.TRUE.equals(userRepository.existsByUserId(userId))) throw new FileTransferException(DBErrorCode.DUPLICATE_ENTITY);
	}
	
	public User getById(Long id) throws FileTransferException {
		if(id == null) return null;
		Optional<User> user = userRepository.findByIdAndIsActive(id, true);
		return user.orElseThrow(() -> new FileTransferException(DBErrorCode.NOT_FOUND));
	}
	
	private User getByUserIdAndPassword(String userId, String password) throws FileTransferException {
		ValidUtil.checkNull(userId, password);
		Optional<User> user = userRepository.findByUserIdAndPasswordAndIsActive(userId, password, true);
		return user.orElseThrow(() -> new FileTransferException(DBErrorCode.NOT_FOUND));
	}
	
	private UserDto userSave(User user) {
		userRepository.save(user);
		return userMapper.toUserDto(user);
	}
	
	public UserDto login(String userId, String password) throws FileTransferException {
		User user = getByUserIdAndPassword(userId, password);
		return userMapper.toUserDto(user);
	}
	
	public UserDto get(Long id) throws FileTransferException {
		User user = getById(id);
		return userMapper.toUserDto(user);
	}
	
	public UserDto toUserDto(UserInsertDto requestDto) {
		return userMapper.toUserDto(requestDto);
	}
	
	public UserDto toUserDto(UserUpdateDto requestDto) {
		return userMapper.toUserDto(requestDto);
	}
	
	public UserDto insert(UserDto requestDto, Department department) {
		User user = userMapper.toUser(requestDto, department);
		return userSave(user);
	}
	
	public UserDto updateByType(Long id, UserDto requestDto, String type) throws FileTransferException {
		User user = getById(id);
		user = userMapper.updateUser(user, requestDto, type);
		return userSave(user);
	}
	
	public Long updateAllDepartment(Long departmentId, Department department) {
		return userRepository.updateAllDepartment(departmentId, department);
	}
	
	public UserDto delete(Long id) throws FileTransferException {
		User user = getById(id);
		user.updateActive(false);
		return userSave(user);
	}
	
	public void initAdmin(String userId, String password) {
		if(Boolean.FALSE.equals(userRepository.existsByUserId(userId))) {
			UserDto userDto = UserDto.builder()
								.userId(userId)
								.password(password)
								.role(Role.ADMIN)
								.name("관리자")
								.build();
			insert(userDto, null);
		}
	}
}
