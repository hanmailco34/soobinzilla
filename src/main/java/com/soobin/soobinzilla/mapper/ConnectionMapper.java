package com.soobin.soobinzilla.mapper;

import org.springframework.stereotype.Component;

import com.soobin.soobinzilla.dto.response.ConnectionConfigDto;
import com.soobin.soobinzilla.dto.response.ConnectionDto;
import com.soobin.soobinzilla.dto.response.FtpConfigDto;
import com.soobin.soobinzilla.dto.response.ProtocolConfigDto;
import com.soobin.soobinzilla.dto.response.SftpConfigDto;
import com.soobin.soobinzilla.dto.response.SmbConfigDto;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.model.Connection;
import com.soobin.soobinzilla.model.ConnectionConfig;
import com.soobin.soobinzilla.model.FtpConfig;
import com.soobin.soobinzilla.model.ProtocolConfig;
import com.soobin.soobinzilla.model.SftpConfig;
import com.soobin.soobinzilla.model.SmbConfig;
import com.soobin.soobinzilla.model.enums.ProtocolType;
import com.soobin.soobinzilla.util.ValidUtil;

@Component
public class ConnectionMapper {

	public Connection toConnection(ConnectionDto connectionDto) throws FileTransferException {
		ValidUtil.checkNull(connectionDto.getHost(), connectionDto.getPassword(), connectionDto.getPort(), connectionDto.getUsername());
		ConnectionConfig connectionConfig = toConnectionConfig(connectionDto.getConnectionConfig());
		return Connection.builder()
				.host(connectionDto.getHost())
				.password(connectionDto.getPassword())
				.port(connectionDto.getPort())
				.username(connectionDto.getUsername())
				.connectionConfig(connectionConfig)								
				.build();
	}
	
	private ConnectionConfig toConnectionConfig(ConnectionConfigDto connectionConfigDto) throws FileTransferException {
		ValidUtil.checkNull(connectionConfigDto.getIsDownload(), connectionConfigDto.getIsRecursive(), connectionConfigDto.getIsThread(), connectionConfigDto.getLocalDirectory(), connectionConfigDto.getServerDirectory());
		ProtocolConfig protocolConfig = toProtocolConfig(connectionConfigDto.getProtocolConfig(), connectionConfigDto.getProtocol());
		ConnectionConfig connectionConfig = ConnectionConfig.builder()
			.protocol(connectionConfigDto.getProtocol())
			.serverDirectory(connectionConfigDto.getServerDirectory())
			.localDirectory(connectionConfigDto.getLocalDirectory())
			.isThread(connectionConfigDto.getIsThread())
			.isRecursive(connectionConfigDto.getIsRecursive())
			.isDownload(connectionConfigDto.getIsDownload())
			.protocolConfig(protocolConfig)
			.build();
		if(protocolConfig != null) protocolConfig.setConnectionConfig(connectionConfig);
		return connectionConfig;
	}
	
	public ProtocolConfig toProtocolConfig(ProtocolConfigDto protocolConfigDto, ProtocolType type) throws FileTransferException {
		switch (type) {
			case FTP: {
				FtpConfigDto dto = (FtpConfigDto) protocolConfigDto;
				return toProtocolConfig(dto);
			}
			case SFTP: {
				SftpConfigDto dto = (SftpConfigDto) protocolConfigDto;
				return toProtocolConfig(dto);
			}
			case SMB: {
				SmbConfigDto dto = (SmbConfigDto) protocolConfigDto;
				return toProtocolConfig(dto);
			}
			default:
				return null;
		}
	}
	
	private ProtocolConfig toProtocolConfig(FtpConfigDto configDto) throws FileTransferException {
		ValidUtil.checkNull(configDto.getAutoDetectUtf8(), configDto.getConnectTime(), configDto.getDataTime(), configDto.getEncoding(), configDto.getFileTransferMode(), configDto.getFileType(), configDto.getPassiveMode());
		return FtpConfig.builder()
			.autoDetectUtf8(configDto.getAutoDetectUtf8())
			.encoding(configDto.getEncoding())
			.fileType(configDto.getFileType())
			.fileTransferMode(configDto.getFileTransferMode())
			.passiveMode(configDto.getPassiveMode())
			.connectTime(configDto.getConnectTime())
			.dataTime(configDto.getDataTime())
			.build();			
	}
	
	private ProtocolConfig toProtocolConfig(SftpConfigDto configDto) throws FileTransferException {
		ValidUtil.checkNull(configDto.getHostKeyEnabled());
		return SftpConfig.builder()
				.hostKeyEnabled(configDto.getHostKeyEnabled())
				.build();
	}
	
	private ProtocolConfig toProtocolConfig(SmbConfigDto configDto) throws FileTransferException {
		ValidUtil.checkNull(configDto.getSection());
		return SmbConfig.builder()
				.domain(configDto.getDomain())
				.section(configDto.getSection())
				.build();
	}
	
	public void toProtocolConfig(ProtocolConfigDto protocolConfigDto, ProtocolConfig protocolConfig, ProtocolType type) {
		switch (type) {
			case FTP: {
				FtpConfigDto dto = (FtpConfigDto) protocolConfigDto;
				FtpConfig config = (FtpConfig) protocolConfig;
				toProtocolConfig(dto, config);
				break;
			}
			case SFTP: {
				SftpConfigDto dto = (SftpConfigDto) protocolConfigDto;
				SftpConfig config = (SftpConfig) protocolConfig;
				toProtocolConfig(dto, config);
				break;
			}
			case SMB: {
				SmbConfigDto dto = (SmbConfigDto) protocolConfigDto;
				SmbConfig config = (SmbConfig) protocolConfig;
				toProtocolConfig(dto, config);
				break;
			}
		}
	}
	
	private void toProtocolConfig(FtpConfigDto configDto, FtpConfig config) {
		config.update(configDto.getEncoding(), configDto.getFileType(), configDto.getFileTransferMode(), configDto.getAutoDetectUtf8(), configDto.getPassiveMode(), configDto.getConnectTime(), configDto.getDataTime());
	}
	
	private void toProtocolConfig(SftpConfigDto configDto, SftpConfig config) {
		config.update(configDto.getHostKeyEnabled());
	}
	
	private void toProtocolConfig(SmbConfigDto configDto, SmbConfig config) {
		config.update(configDto.getDomain(), configDto.getSection());
	}
	
	public ConnectionDto toConnectionDto(Connection connection) {
		ConnectionConfigDto connectionConfigDto = toConnectionConfigDto(connection.getConnectionConfig());
		
		return ConnectionDto.builder()
			.id(connection.getId())
			.host(connection.getHost())
			.password(connection.getPassword())
			.port(connection.getPort())
			.username(connection.getUsername())
			.connectionConfig(connectionConfigDto)								
			.build();
	}
	
	private ConnectionConfigDto toConnectionConfigDto(ConnectionConfig connectionConfig) {
		ProtocolConfigDto protocolConfig = toProtocolConfigDto(connectionConfig.getProtocolConfig(), connectionConfig.getProtocol());
		
		return ConnectionConfigDto.builder()
			.protocol(connectionConfig.getProtocol())
			.serverDirectory(connectionConfig.getServerDirectory())
			.localDirectory(connectionConfig.getLocalDirectory())
			.isThread(connectionConfig.getIsThread())
			.isRecursive(connectionConfig.getIsRecursive())
			.isDownload(connectionConfig.getIsDownload())
			.protocolConfig(protocolConfig)
			.build();
	}
	
	private ProtocolConfigDto toProtocolConfigDto(ProtocolConfig protocolConfig, ProtocolType type) {
		switch (type) {
			case FTP: {
				FtpConfig protocol = (FtpConfig) protocolConfig;
				return toProtocolConfigDto(protocol);
			}
			case SFTP: {
				SftpConfig protocol = (SftpConfig) protocolConfig;
				return toProtocolConfigDto(protocol);
			}
			case SMB: {
				SmbConfig protocol = (SmbConfig) protocolConfig;
				return toProtocolConfigDto(protocol);
			}
			default:
				return null;
		}
	}
	
	private ProtocolConfigDto toProtocolConfigDto(FtpConfig config) {
		return FtpConfigDto.builder()
			.autoDetectUtf8(config.getAutoDetectUtf8())
			.encoding(config.getEncoding())
			.fileType(config.getFileType())
			.fileTransferMode(config.getFileTransferMode())
			.passiveMode(config.getPassiveMode())
			.connectTime(config.getConnectTime())
			.dataTime(config.getDataTime())
			.build();
	}
	
	private ProtocolConfigDto toProtocolConfigDto(SftpConfig config) {
		return SftpConfigDto.builder()
				.hostKeyEnabled(config.getHostKeyEnabled())
				.build();
	}
	
	private ProtocolConfigDto toProtocolConfigDto(SmbConfig config) {
		return SmbConfigDto.builder()
				.domain(config.getDomain())
				.section(config.getSection())
				.build();
	}
	
	public Connection targetConection(String ip, Integer port, String username, String pwd, String serverDirectory, ProtocolType protocol) {
		ProtocolConfig protocolConfig = null;
		if(ProtocolType.FTP.equals(protocol))
			protocolConfig = FtpConfig.builder().build();
		else if(ProtocolType.SFTP.equals(protocol))
			protocolConfig = SftpConfig.builder().build();
		else if(ProtocolType.SMB.equals(protocol))
			protocolConfig = SmbConfig.builder().build();
		
		ConnectionConfig connectionConfig = ConnectionConfig.builder()
				.protocol(protocol)
				.serverDirectory(serverDirectory)
				.localDirectory("")
				.isDownload(false)
				.protocolConfig(protocolConfig)
				.build();
		
		if(protocolConfig != null) protocolConfig.setConnectionConfig(connectionConfig);
		
		return Connection.builder()
			.host(ip)
			.password(pwd)
			.port(port)
			.username(username)
			.connectionConfig(connectionConfig)								
			.build();		
	}
}
