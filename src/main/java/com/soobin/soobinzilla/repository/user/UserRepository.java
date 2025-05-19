package com.soobin.soobinzilla.repository.user;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;

import com.soobin.soobinzilla.model.User;

public interface UserRepository extends JpaRepository<User, Long>, UserRepositotyCustom, QuerydslPredicateExecutor<User> {
	Optional<User> findByIdAndIsActive(Long id, Boolean isActive);
	boolean existsByUserId(String userId);
	Optional<User> findByUserIdAndPasswordAndIsActive(String userId, String password, Boolean isActive);
}
