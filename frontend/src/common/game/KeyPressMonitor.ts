// class to monitor keypress, designed as a singleton
export class KeyPressMonitorBase {
	keypress: Set<KeyboardEvent["key"]> = new Set();
	
	add(value: string): void {
		this.keypress.add(value);
	}
	
	delete(value: string): void {
		this.keypress.delete(value);
	}
	
	// expose the underlying has function of the set
	has(value: string) {
		return this.keypress.has(value);
	}
}


// KeyPressMonitor version meant to be run by client
export default class KeyPressMonitor extends KeyPressMonitorBase {
	private static _instance: KeyPressMonitor;
	private onKeyDown: ((key: KeyboardEvent["key"]) => void) | null
		= null;
	private onKeyUp: ((key: KeyboardEvent["key"]) => void) | null
		= null;
	
	// since class is defined as singleton, make constructor function
	// private to prevent direct construction call
	private constructor() {
		super();
		if (typeof window !== 'undefined') {
			// at client register keydown and keyup events
			window.addEventListener("keydown", this.keydown.bind(this));
			window.addEventListener("keyup", this.keyup.bind(this));
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
		const instance = KeyPressMonitor._instance;
		instance.onKeyDown = onKeyDown;
		instance.onKeyUp = onKeyUp;
		return instance;
	}
	
	// add key to the set of pressed keys
	keydown(e: KeyboardEvent): void {
		// do nothing if key is already pressed
		if (this.has(e.key))
			return ;
		this.keypress.add(e.key);
		if (this.onKeyDown != null)
			this.onKeyDown(e.key);
	}
	
	// removed key to the set of pressed keys
	keyup(e: KeyboardEvent): void {
		this.keypress.delete(e.key);
		if (this.onKeyUp != null)
			this.onKeyUp(e.key);
	}
}

