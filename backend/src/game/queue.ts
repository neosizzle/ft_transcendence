interface IQueue<T> {
	empty(): boolean;
	size(): number;
	front(): T;
	back(): T;
	pop(): void;
}

export default class Queue<T> implements IQueue<T> {
	protected arr = new Array<T>(0);
	
	// Returns whether the queue is empty: i.e. whether its size is zero.
	empty(): boolean {
		return this.arr.length == 0;
	}
	
	// Returns the number of elements in the queue.
	size(): number {
		return this.arr.length;
	}
	
	// Returns the next element in the queue.
	front(): T {
		return this.arr[0];
	}
	
	// Returns the last element pushed into the queue.
	back(): T {
		return this.arr[this.size() - 1];
	}
	
	// Inserts a new element at the end of the queue.
	push(val: T): void {
		this.arr.push(val);
	}
	
	// Removes the next element in the queue, reducing its size by one.
	pop(): void {
		this.arr.shift();
	}
	
	// Removes element 'val' from the queue, and returns the number of elements
	// erased.
	erase(val: T): number {
		let retval = 0;
		let index = this.arr.indexOf(val);
		while (index > -1) {
			this.arr.splice(index, 1);
			++retval;
			index = this.arr.indexOf(val, index);
		}
		return retval;
	}
	
	// Returns the first (least) index of an element within the array equal to
	// the specified value, or -1 if none is found.
	indexOf(val: T): number {
		return this.arr.indexOf(val);
	}
}


// A queue with non-repeating entry
export class UniqueQueue<T> extends Queue<T> {
	private elem = new Set<T>();	// keep track if element already in queue

	constructor() {
		super();
	}
	
	// Inserts a new element at the end of the queue if it doesn't exist.
	push(val: T): void {
		if (this.elem.has(val))
			return ;
		this.elem.add(val);
		this.arr.push(val);
	}
	
	// Removes the next element in the queue, reducing its size by one.
	pop(): void {
		const val: T = this.front();
		if (val != null) {
			this.elem.delete(val);
			super.pop();
		}
	}
	
	erase(val: T): number {
		let retval = 0;
		const index = this.arr.indexOf(val);
		if (index != -1) {
			this.arr.splice(index, 1);
			++retval;
		}
		this.elem.delete(val);
		return retval;
	}
}


// Simplified implementation of C++ std::pair
class Pair<T1, T2> {
	first: T1;
	second: T2;
	
	constructor(key: T1, val: T2) {
		this.first = key;
		this.second = val;
	}
}


export class UniqueKeyValueQueue<Key, T> implements IQueue<Pair<Key, T>> {
	private queue = new Queue<Key>();
	private map = new Map<Key, T>();
	
	// Returns whether the queue is empty: i.e. whether its size is zero.
	empty(): boolean {
		return this.queue.empty();
	}
	
	// Returns the number of elements in the queue.
	size(): number {
		return this.queue.size();
	}
	
	// Returns the next element in the queue.
	front(): Pair<Key, T> {
		const key: Key = this.queue.front();
		if (key != null)
			return new Pair(key, this.map.get(key));
		else
			return undefined;
	}
	
	// Returns the last element pushed into the queue.
	back(): Pair<Key, T> {
		const key: Key = this.queue[this.size() - 1];
		if (key != null)
			return new Pair(key, this.map.get(key));
		else
			return undefined;
	}
	
	// Inserts a new element at the end of the queue if it doesn't exist.
	push(key: Key, val: T): void {
		if (this.map.has(key))	// don't allow to add to map
			return ;
		this.map.set(key, val);
		this.queue.push(key);
	}
	
	// Removes the next element in the queue, reducing its size by one.
	pop(): void {
		const key: Key = this.queue.front();
		if (key != null) {
			this.map.delete(key);
			this.queue.pop();
		}
	}
	
	// Removes element 'key' from the queue, and returns the number of elements
	// erased.
	erase(key: Key): number {
		if (this.map.has(key) == false)
			return 0;
		this.map.delete(key);
		return this.queue.erase(key);
	}
	
	// Returns the first (least) index of an element within the array equal to
	// the specified value, or -1 if none is found.
	indexOf(key: Key): number {
		return this.queue.indexOf(key);
	}
}
