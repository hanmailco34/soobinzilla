package com.soobin.soobinzilla.model;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.MappedSuperclass;

import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import lombok.Getter;

@Getter
@MappedSuperclass
@EntityListeners(AuditingEntityListener.class)
public abstract class BaseTime {

	@CreatedDate
	@Column(nullable = false, updatable = false)
	private LocalDateTime createAt;
	
	@LastModifiedDate
	@Column(nullable = false)
	private LocalDateTime updateAt;
}
