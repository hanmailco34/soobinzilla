package com.soobin.soobinzilla.service;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.soobin.soobinzilla.dto.response.ConnectionConfigDto;
import com.soobin.soobinzilla.dto.response.ConnectionDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.DBErrorCode;
import com.soobin.soobinzilla.mapper.ConnectionMapper;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.ConnectionConfig;
import com.soobin.soobinzilla.model.ProtocolConfig;
import com.soobin.soobinzilla.model.enums.ProtocolType;
import com.soobin.soobinzilla.repository.connection.ConnectionRepository;
import com.soobin.soobinzilla.util.ConstantUtil;
import com.soobin.soobinzilla.util.FileUtil;
import com.soobin.soobinzilla.util.ObjectUtil;
import com.soobin.soobinzilla.util.TimeUtil;
import com.soobin.soobinzilla.util.ValidUtil;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ConnectionService {

	private final ConnectionRepository connectionRepository;
	
	private final ConnectionMapper connectionMapper;
	
	@Value("${app.log.base-dir}")
	private String baseDir;
	
	@Value("${app.log.connection-dir}")
	private String connectionDir;
	
	@Value("${app.log.connection-file}")
	private String connectionFileName;
	
	public Connection getById(Long id) throws FileTransferException {
		ValidUtil.checkNull(id);
		Optional<Connection> connection = connectionRepository.findById(id);
		return connection.orElseThrow(() -> new FileTransferException(DBErrorCode.NOT_FOUND));
	}
	
	public ConnectionDto insert(ConnectionDto connectionDto) throws FileTransferException {
		Connection connection = connectionMapper.toConnection(connectionDto);
		connectionRepository.save(connection);
		return connectionMapper.toConnectionDto(connection);
	}
	
	public ConnectionDto update(ConnectionDto connectionDto) throws FileTransferException {
		Connection connection = getById(connectionDto.getId());
		
		ConnectionConfig connectionConfig = connection.getConnectionConfig();
		ConnectionConfigDto connectionConfigDto = connectionDto.getConnectionConfig();
		ProtocolConfig protocolConfig = null;
		// 프로토콜의 변화가 없으면 원래 있는 옵션을 변경 
		if(connectionConfig.getProtocol().equals(connectionConfigDto.getProtocol())) {
			protocolConfig = connectionConfig.getProtocolConfig();
			connectionMapper.toProtocolConfig(connectionDto.getConnectionConfig().getProtocolConfig(), protocolConfig, connectionConfig.getProtocol());
		} else {
			protocolConfig = connectionMapper.toProtocolConfig(connectionDto.getConnectionConfig().getProtocolConfig(), connectionDto.getConnectionConfig().getProtocol());
			protocolConfig.setConnectionConfig(connectionConfig);
		}
		
		connectionConfig.update(connectionConfigDto.getProtocol(), connectionConfigDto.getServerDirectory(), connectionConfigDto.getLocalDirectory(), connectionConfigDto.getIsThread(), connectionConfigDto.getIsRecursive(), protocolConfig);
		connection.update(connectionDto.getHost(), connectionDto.getPort(), connectionDto.getUsername(), connectionDto.getPassword(), connectionDto.getIsSecure(), connectionConfig);
		connectionRepository.save(connection);
		
		return connectionMapper.toConnectionDto(connection);
	}
	
	public void delete(Long id) throws FileTransferException {
		Connection connection = getById(id);
		connectionRepository.delete(connection);
	}
	
	public List<Connection> findByIsDownload(Boolean isDownload) {
		return connectionRepository.findByIsDownload(isDownload);
	}
	
	public void initConnection(String ip, Integer port, String username, String pwd, String serverDirectory, ProtocolType protocol) {
		List<Connection> targetList = findByIsDownload(false);
		if(Boolean.TRUE.equals(ObjectUtil.isEmpty(targetList))) {
			Connection target = connectionMapper.targetConection(ip, port, username, pwd, serverDirectory, protocol);
			connectionRepository.save(target);
		}
	}
	
	public List<String> getLog(Long id) throws FileTransferException {
		return getLog(id, connectionFileName + ".log", false);
	}
	
	public List<String> getLog(Long id, Date date) throws FileTransferException {
		String dateString = TimeUtil.toStringDate(date, ConstantUtil.CONNECTION_DATE_LOG_PATTERN);
		
		return getLog(id, connectionFileName + "-" + dateString + ".log.gz", true);
	}
	
	private List<String> getLog(Long id, String fileName, Boolean isGzip) throws FileTransferException {
		String logPath = String.format("%s/%s/%d/%s", baseDir, connectionDir, id, fileName);
		return Boolean.TRUE.equals(isGzip) ? FileUtil.readGzipFile(logPath) : FileUtil.readFile(logPath);
	}
}
