// class to monitor keypress, designed as a singleton
export default class KeyPressMonitor {
	private static _instance: KeyPressMonitor;
	static keypress: Set<KeyboardEvent["key"]> = new Set();
	
	// since class is defined as singleton, make constructor function
	// private to prevent direct construction call
	private constructor() {
		// register keydown and keyup events
		window.addEventListener("keydown", KeyPressMonitor.keydown);
		window.addEventListener("keyup", KeyPressMonitor.keyup);
	}
	
	// create, if necessary, and return the singleton object
	public static get_instance(): KeyPressMonitor {
		if (!KeyPressMonitor._instance) {
			KeyPressMonitor._instance = new KeyPressMonitor();
		}
		return KeyPressMonitor._instance;
	}
	
	// add key to the set of pressed keys
	private static keydown(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.add(e.key);
	}
	
	// removed key to the set of pressed keys
	private static keyup(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.delete(e.key);
	}
	
	// expose the underlying has function of the set
	public static has(value: string) {
		return KeyPressMonitor.keypress.has(value);
	}
}
