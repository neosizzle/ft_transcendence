import { Socket, Server } from 'socket.io';

import Pong, { GameInterface } from '../common/game/Pong';
import { KeyPressMonitorBase } from '../common/game/KeyPressMonitor';
import { UniqueQueue } from './queue';

type QueueInfo =  {
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
		if (index == 0)
			client.emit('join', n);	// ask client to join as current player
		this.server.emit("updateQueue");	// ask clients to update queue info
		return index;
	}
	
	// A client disconnects. Check whether client is in queue. If yes,
	// the client is removed from queue. If the client is also the current
	// player, release any key that he may be pressing.
	handleDisconnect(client: Socket): void {
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueQueue<Socket> = this.queues[n];
			const index: number = queue.indexOf(client);
			if (index == -1)
				continue ;
			queue.erase(client);	// remove client from queue
			console.log(`client ${client.id} removed as player ${n}`);
			if (index == 0)		// if client is player, release keys
				for (const key of Object.values(this.game.control_keys[n])) {
					this.keypress.delete(key);
			}
			// ask clients to update their queue info
			this.server.emit('updateQueue');
		}
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
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueQueue<Socket> = this.queues[n];
			if (queue.size() > 0) {
				const client: Socket = queue.front();
				client.emit('unjoin', n);	// unjoin as player i
				queue.pop();
				this.game.unset_player(n);
			}
			
			// ask the next player in the queue to join
			if (queue.front() != null)
				queue.front().emit("join", n)
		}
		
		// ask clients to update queue info
		this.server.emit("updateQueue");
	}
	
	// return the queue info of a client
	getQueue(client: Socket): QueueInfo {
		return {
			position: this.queues.map((queue) => queue.indexOf(client)),
			size: this.queues.map((queue) => queue.size()),
		}
	}
	
}
