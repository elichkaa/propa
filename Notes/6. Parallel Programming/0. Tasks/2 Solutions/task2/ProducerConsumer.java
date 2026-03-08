package producerconsumer;

public class ProducerConsumer {

	public static void main(String[] args) {

		Buffer buffer = new Buffer();

		Thread producer1Thread = new Thread(new Producer(buffer));
		Thread producer2Thread = new Thread(new Producer(buffer));
		Thread consumerThread = new Thread(new Consumer(buffer));

		producer1Thread.start();
		producer2Thread.start();
		consumerThread.start();

		try {
			Thread.sleep(10000);
		} catch (InterruptedException e1) {
			buffer.stopRunning();
		} finally {
			buffer.stopRunning();
		}

		producer1Thread.interrupt();
		producer2Thread.interrupt();
		consumerThread.interrupt();

		try {
			producer1Thread.join();
			producer2Thread.join();
			consumerThread.join();
		} catch (InterruptedException e2) {

		}
	}

}
