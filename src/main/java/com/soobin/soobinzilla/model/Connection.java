package com.soobin.soobinzilla.model;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "zilla_connection")
public class Connection extends BaseTime {
	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
	
	@Column(nullable = false)
    private String host;
    
    @Column(nullable = false)
    private Integer port;
    
    @Column(nullable = false)
    private String username;
    
    @Column(nullable = false)
    private String password;
    
    @Column(nullable = false)
	@Builder.Default()
    private Boolean isSecure = false;
    
    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "connection_config_id")
    private ConnectionConfig connectionConfig;
    
    @OneToOne(mappedBy = "connection")
	private Schedule schedule;
    
    public void update(String host, Integer port, String username, String password, Boolean isSecure, ConnectionConfig connectionConfig) {
    	this.host = host != null ? host : this.host;
        this.port = port != null ? port : this.port;
        this.username = username != null ? username : this.username;
        this.password = password != null ? password : this.password;
        this.isSecure = isSecure != null ? isSecure : this.isSecure;
        this.connectionConfig = connectionConfig != null ? connectionConfig : this.connectionConfig;
    }
}
