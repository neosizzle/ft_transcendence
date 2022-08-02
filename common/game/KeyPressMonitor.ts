// class to monitor keypress, designed as a singleton
export class KeyPressMonitorBase {
	protected static _instance: KeyPressMonitorBase;
	static keypress: Set<KeyboardEvent["key"]> = new Set();
	
	public static add(value: string): void {
		KeyPressMonitorBase.keypress.add(value);
	}
	
	public static delete(value: string): void {
		KeyPressMonitorBase.keypress.delete(value);
	}
	
	// expose the underlying has function of the set
	public static has(value: string) {
		return KeyPressMonitorBase.keypress.has(value);
	}
}


// KeyPressMonitor version meant to be run by client
export default class KeyPressMonitor extends KeyPressMonitorBase {
	private static onKeyDown: ((key: KeyboardEvent["key"]) => void) | null
		= null;
	private static onKeyUp: ((key: KeyboardEvent["key"]) => void) | null
		= null;
	
	// since class is defined as singleton, make constructor function
	// private to prevent direct construction call
	private constructor() {
		super();
		if (typeof window !== 'undefined') {
			// at client register keydown and keyup events
			window.addEventListener("keydown", KeyPressMonitor.keydown);
			window.addEventListener("keyup", KeyPressMonitor.keyup);
		}
	}
	
	// create, if necessary, and return the singleton object
	public static get_instance(
			onKeyDown: ((key: KeyboardEvent["key"]) => void),
			onKeyUp: ((key: KeyboardEvent["key"]) => void)
			): KeyPressMonitor {
		if (!KeyPressMonitor._instance) {
			KeyPressMonitor._instance = new KeyPressMonitor();
		}
		KeyPressMonitor.onKeyDown = onKeyDown;
		KeyPressMonitor.onKeyUp = onKeyUp;
		return KeyPressMonitor._instance;
	}
	
	// add key to the set of pressed keys
	public static keydown(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.add(e.key);
		if (KeyPressMonitor.onKeyDown != null)
			KeyPressMonitor.onKeyDown(e.key);
	}
	
	// removed key to the set of pressed keys
	public static keyup(e: KeyboardEvent): void {
		KeyPressMonitor.keypress.delete(e.key);
		if (KeyPressMonitor.onKeyUp != null)
			KeyPressMonitor.onKeyUp(e.key);
	}
}

