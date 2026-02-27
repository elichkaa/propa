package producerconsumer;

public class Producer implements Runnable {

	Buffer buffer;

	public Producer(Buffer buffer) {
		this.buffer = buffer;
	}

	@Override
	public void run() {
		// Allow to shutdown a producer by stopping the buffer or by sending an interrupt
		while (buffer.isRunning() && !Thread.currentThread().isInterrupted()) {
			buffer.produce();
		}
	}
}
