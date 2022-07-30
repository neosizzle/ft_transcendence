import { Socket } from 'socket.io-client';


// class to monitor keypress, designed as a singleton
export default class KeyPressMonitor {
	private static _instance: KeyPressMonitor;
	private static keypress: Set<KeyboardEvent["key"]> = new Set();
	private static socket: Socket | null = null;
	
	// since class is defined as singleton, make constructor function
	// private to prevent direct construction call
	private constructor() {
		if (typeof window !== 'undefined') {
			// at client register keydown and keyup events
			window.addEventListener("keydown", KeyPressMonitor.keydown);
			window.addEventListener("keyup", KeyPressMonitor.keyup);
		}
	}
	
	// create, if necessary, and return the singleton object
	public static get_instance(socket?: Socket): KeyPressMonitor {
		if (!KeyPressMonitor._instance) {
			KeyPressMonitor._instance = new KeyPressMonitor();
		}
		const instance: KeyPressMonitor = KeyPressMonitor._instance;
		if (typeof socket !== "undefined") {
			instance.add_socket(socket);
		}
		return instance;
	}
	
	// add socket to the instance
	add_socket(socket: Socket) {
		if (typeof socket !== "undefined" && socket != null) {
			KeyPressMonitor.socket = socket;
			console.log("added socket:", typeof KeyPressMonitor.socket)
		}
	}
	
	// add key to the set of pressed keys
	public static keydown(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.add(e.key);
		if (KeyPressMonitor.socket != null)
			KeyPressMonitor.socket.emit("keydown", e.key);
	}
	
	// removed key to the set of pressed keys
	public static keyup(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.delete(e.key);
		if (KeyPressMonitor.socket != null)
			KeyPressMonitor.socket.emit("keyup", e.key);
	}
	
	public static add(value: string): void {
		KeyPressMonitor.keypress.add(value);
	}
	
	public static delete(value: string): void {
		KeyPressMonitor.keypress.delete(value);
	}
	
	// expose the underlying has function of the set
	public static has(value: string) {
		return KeyPressMonitor.keypress.has(value);
	}
}
