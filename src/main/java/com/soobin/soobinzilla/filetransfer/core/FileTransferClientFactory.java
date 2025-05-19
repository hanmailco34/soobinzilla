package com.soobin.soobinzilla.filetransfer.core;

import java.util.function.Supplier;

public class FileTransferClientFactory<T> {

	private T client;
	private Supplier<T> cliSupplier;
	
	protected FileTransferClientFactory(Supplier<T> cliSupplier) {
		this.cliSupplier = cliSupplier;
	}
	
	public T getClient() {
		if(this.client == null) {
			this.client = cliSupplier.get();
		}
		return this.client;
	}
	
	public T resetClient() {
		this.client = cliSupplier.get();
		return this.client;
	}
}
