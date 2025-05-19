package com.soobin.soobinzilla.filetransfer.core;

import com.hierynomus.smbj.SmbConfig;
import com.soobin.soobinzilla.exception.FileTransferException;
import com.soobin.soobinzilla.exception.code.ConnectionErrorCode;
import com.soobin.soobinzilla.filetransfer.transfer.FTPFileTransfer;
import com.soobin.soobinzilla.filetransfer.transfer.SFTPFileTransfer;
import com.soobin.soobinzilla.filetransfer.transfer.SMBFileTransfer;
import com.soobin.soobinzilla.model.enums.ProtocolType;

public class FileTransferManager {
	public static IFileTransferProtocol getProtocol(ProtocolType protocolType) throws FileTransferException {
		if(protocolType == null) throw new FileTransferException(ConnectionErrorCode.INVALID_PARAMETER);
		switch (protocolType) {
			case FTP:
				return new FTPFileTransfer();
			case SFTP:
				return new SFTPFileTransfer();
			case SMB:
				return new SMBFileTransfer();
			default:
				throw new FileTransferException(ConnectionErrorCode.UNSUPPORTED_PROTOCOL);
		}
}

	public static IFileTransferProtocol getSMBConfigProtocol(SmbConfig config) throws FileTransferException {
		return new SMBFileTransfer(config);
	}
}
