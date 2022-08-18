import { Socket, Server } from 'socket.io';

import Pong, { GameInterface } from '../common/game/Pong';
import { KeyPressMonitorBase } from '../common/game/KeyPressMonitor';
import { UniqueQueue } from './queue';

export type QueueInfo =  {
	position: number[],
	size: number[]
}

export default class GameServer {
	server: Server;
	queues: UniqueQueue<Socket>[] =
		[new UniqueQueue<Socket>(), new UniqueQueue<Socket>()];
	keypress: KeyPressMonitorBase;
	game: GameInterface;
	
	constructor(server: Server) {
		this.server = server;
		this.keypress = new KeyPressMonitorBase();
		this.game = new Pong(400, 300, 2,
			this.updateClient.bind(this), this.onGameEnd.bind(this));
		
		// run the game in an infinite loop at 60 Hz
		setInterval(this.game.update.bind(this.game), 1000/60);
	}
	
	// a new spectator joins. Broadcast game state to it.
	spectate(client: Socket) {
		client.emit('game_state', this.game.get_state());
		client.emit("updateQueue");	// ask clients to update queue info
	}
	
	
	// 'client' attempts to join queue 'n'. Return the position in queue.
	handleConnect(client: Socket, n: number): number {
		const queue: UniqueQueue<Socket> = this.queues[n];
		queue.push(client);
		const index: number = queue.indexOf(client);
		console.log(`client ${client.id} joined queue ${n} at index ${index}`);
		this.server.emit("updateQueue");	// ask clients to update queue info
		return index;
	}
	
	/* Handles client disconnection
	 * Check whether client is in queue. If yes, remove from queue.
	 * If client is also the current player, the current game ends with both
	 * current players removed from the game.
	 */
	handleDisconnect(client: Socket, queue_no?: number): void {
		// if queue_no is not given, loop through all queues until client is
		// found
		let index: number;
		for (let n = 0; n < this.queues.length; ++n) {
			index = this.queues[n].indexOf(client);
			if (index != -1) {
				queue_no = n;
				break ;
			}
		}
		
		// client is not in any queue. Do nothing.
		if (index == -1)
			return ;
		
		if (index > 0) {
			// remove non-current player from queue
			this.queues[queue_no].erase(client);
			console.log(`client ${client.id} removed from queue ${queue_no}`);
		} else if (index == 0) {
			// end current game and handle players removal
			this.onGameEnd();
		}
		
		// ask clients to update their queue info and game state
		this.server.emit('updateQueue');
		this.updateClient()
	}
	
	/* 'client' equests to start game. Server checks whether client is a
	 * current player. */
	start(client: Socket) {
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueQueue<Socket> = this.queues[n];
			if (client != queue.front())
				continue ;
			this.game.start(n);	// game records that that player is ready
			console.log(`Player ${n} pressed start.`)
			this.game.control(this.keypress.keypress);
		}
	}
	
	// server receives key down event
	keyDown(client: Socket, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueQueue<Socket> = this.queues[n];
			if (client == queue.front()
					&& Object.values(this.game.control_keys[n]).indexOf(key)
						!= -1) {
				console.log(`Received keydown ${key} from player ${n}`);
				this.keypress.add(key);
				this.game.control(this.keypress.keypress);
				break ;
			}
		}
	}
	
	// server receives key up event
	keyUp(client: Socket, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueQueue<Socket> = this.queues[n];
			if (client == queue.front()
					&& Object.values(this.game.control_keys[n]).indexOf(key)
						!= -1) {
				console.log(`Received keyup ${key} from player ${n}`);
				this.keypress.delete(key);
				this.game.control(this.keypress.keypress);
				break ;
			}
		}
	}
	
	// callback function to update clients of the latest game state
	updateClient(): void {
		this.server.emit('game_state', this.game.get_state());
	}
	
	// game has ended, so remove existing players from game
	onGameEnd(): void {
		console.log("Game has ended!");
		
		// record one of the players as winner
		/* Insert code here */
		
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueQueue<Socket> = this.queues[n];
			this.game.unset_player(n);  // remove player from current game
			if (queue.size() > 0) {
				const client: Socket = queue.front();
				client.emit('unjoin', n);	// unjoin as player n
				queue.pop();
			}
		}
		
		this.keypress.keypress = new Set();	// release all keys
		this.game.reset(true);	// reset game into a new game
		
		// ask the next players in the queue to join
		for (let n = 0; n < this.queues.length; ++n) {
			const client: Socket = this.queues[n].front()
			if (client != null)
				client.emit("join", n)
		}
	}
	
	// return the queue info of a client
	getQueue(client: Socket): QueueInfo {
		const retval = {
			position: this.queues.map((queue) => queue.indexOf(client)),
			size: this.queues.map((queue) => queue.size()),
		}
		
		// if client is the current player, ask it to join game
		for (let i = 0; i < retval.position.length; ++i)
			if (retval.position[i] == 0)
				client.emit('join', i)	// ask client to join as player i
		
		return retval;
	}
}
