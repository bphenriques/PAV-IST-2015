package lab01;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class HelloWorld implements Message {

	public static void main(String[] args) {
		try {
			String messageClass;
			BufferedReader br = new BufferedReader(new InputStreamReader(
					System.in));
			System.out.println("Enter class Name (lab01.HelloWorld or lab01.GoodbyeWorld)");
			messageClass = br.readLine();
			System.out.println(messageClass);
			
			Class interfaceCalled = Class.forName(messageClass);
			
			Message message = (Message)interfaceCalled.newInstance();
			message.say();
			
			
		} catch (IOException e) {
			System.out.println("Rebentou");
			System.exit(-1);
		} catch (ClassNotFoundException e) {
			System.out.println("Deu erro, chama o prof");
			System.exit(-1);
		} catch (InstantiationException e) {
			System.out.println("Deu erro, chama o prof");
			System.exit(-1);
		} catch (IllegalAccessException e) {
			System.out.println("Deu erro, chama o prof");
			System.exit(-1);
		}
	}

	@Override
	public void say() {
		System.out.println("Hello World!");
	}

}
