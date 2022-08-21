import { Socket, Server } from 'socket.io';

import Pong, { GameInterface } from '../common/game/Pong';
import { KeyPressMonitorBase } from '../common/game/KeyPressMonitor';
import { UniqueKeyValueQueue } from './queue';
import { UsersService } from 'src/users/users/users.service';
import { UserStatus } from '@prisma/client';


export type QueueInfo =  {
	position: number[],
	size: number[],
	player: string[],
}

export default class GameServer {
	server: Server;
	usersService: UsersService;
	queues: UniqueKeyValueQueue<string, Socket>[] = [
		new UniqueKeyValueQueue<string, Socket>(),
		new UniqueKeyValueQueue<string, Socket>()
		];
	keypress: KeyPressMonitorBase;
	game: GameInterface;
	
	constructor(server: Server, usersService: UsersService) {
		this.server = server;
		this.usersService = usersService;
		this.keypress = new KeyPressMonitorBase();
		this.game = new Pong(400, 300,
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
		const queue: UniqueKeyValueQueue<string, Socket> = this.queues[n];
		const id: string = client.handshake.auth.user.id;
		queue.push(id, client);
		const index: number = queue.indexOf(id);
		// set client status as INGAME if he is a current player
		if (index == 0) {
			this.usersService.patchMe(
				client.handshake.auth.user, {status: UserStatus.INGAME}
			)
		}
		console.log(`client ${id} joined queue ${n} at index ${index}`);
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
			const id: string = client.handshake.auth.user?.id;
			index = this.queues[n].indexOf(id);
			if (index != -1) {
				queue_no = n;
				break ;
			}
		}
		
		// client is not in any queue. Do nothing.
		if (index == -1)
			return ;
		
		const id: string = client.handshake.auth.user.id;
		if (index > 0) {
			// remove non-current player from queue
			this.queues[queue_no].erase(id);
			console.log(`client ${id} removed from queue ${queue_no}`);
		} else if (index == 0) {
			// end current game and handle players removal
			this.onGameEnd();
		}
		
		// ask clients to update their queue info and game state
		this.server.emit('updateQueue');
		this.updateClient()
	}
	
	/* 'client' requests to start game. Server checks whether client is a
	 * current player. */
	start(client: Socket) {
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueKeyValueQueue<string, Socket> = this.queues[n];
			const id: string = client.handshake.auth.user.id;
			if (id != queue.front()?.first)
				continue ;
			this.game.start(n);	// game records that that player is ready
			console.log(`Player ${n} pressed start.`)
			this.game.control(this.keypress.keypress);
		}
	}
	
	// server receives key down event
	keyDown(client: Socket, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueKeyValueQueue<string, Socket> = this.queues[n];
			const id: string = client.handshake.auth.user.id;
			if (id == queue.front()?.first
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
			const queue: UniqueKeyValueQueue<string, Socket> = this.queues[n];
			const id: string = client.handshake.auth.user.id;
			if (id == queue.front()?.first
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
		this.server.emit('game_type', this.game.type);
	}
	
	// game has ended, so remove existing players from game
	onGameEnd(): void {
		console.log("Game has ended!");
		
		// record one of the players as winner
		/* Insert code here */
		
		for (let n = 0; n < this.queues.length; ++n) {
			const queue: UniqueKeyValueQueue<string, Socket> = this.queues[n];
			this.game.unset_player(n);  // remove player from current game
			if (queue.size() > 0) {
				const client: Socket = queue.front().second;
				
				// change user status from INGAME to LOGGEDIN
				this.usersService.patchMe(
					client.handshake.auth.user, {status: UserStatus.LOGGEDIN}
				)
				
				client.emit('unjoin', n);	// unjoin as player n
				queue.pop();
			}
		}
		
		this.keypress.keypress = new Set();	// release all keys
		this.game.reset(true);	// reset game into a new game
		
		// ask the next players in the queue to join
		for (let n = 0; n < this.queues.length; ++n) {
			const client: Socket = this.queues[n].front()?.second
			if (client != null) {
				client.emit("join", n)
				this.usersService.patchMe(
					client.handshake.auth.user, {status: UserStatus.INGAME}
				)
			}
		}
	}
	
	// return the queue info of a client
	getQueue(client: Socket): QueueInfo {
		const retval = {
			position: this.queues.map(
				(queue) => queue.indexOf(client.handshake.auth.user.id)),
			size: this.queues.map((queue) => queue.size()),
			player: this.queues.map(
				(queue) => queue.front() ?
					queue.front().second.handshake.auth.user.intraName : ""
			),
		}
		
		// if client is the current player, ask it to join game
		for (let i = 0; i < retval.position.length; ++i)
			if (retval.position[i] == 0) {
				client.emit('join', i)	// ask client to join as player i
				this.usersService.patchMe(
					client.handshake.auth.user, {status: UserStatus.INGAME}
				)
			}
		return retval;
	}
	
	// if type is true, change to a customised game; else change to
	// original game.
	setGameType(type: boolean): void {
		this.game.set_type(type);
		this.server.emit('game_type', type);
	}
	
	getGameType(): boolean {
		return this.game.type;
	}
}
