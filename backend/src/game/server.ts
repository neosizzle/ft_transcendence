import { Socket, Server } from 'socket.io';

import Pong, { GameInterface } from '../common/game/Pong';
import { KeyPressMonitorBase } from '../common/game/KeyPressMonitor';


export default class GameServer {
	server: Server;
	players: Socket[] = [null, null];	// records player sockets
	game: GameInterface;
	
	constructor(server: Server) {
		this.server = server;
		this.game = new Pong(400, 300, 2, this.onGameEnd.bind(this));
		
		// run the game in an infinite loop at 60 Hz
		setInterval(this.game.update.bind(this.game), 1000/60);
	}
	
	// a new spectator joins. Broadcast game state to it.
	spectate(client: Socket) {
		client.emit('game_state', this.game.get_state());
	}
	
	
	/* client 'id' attemps to join as player 'n'. Return 'n' upon
	 * success, or -1 upon failure. */ 
	connect(client: Socket, n: number): number {
		// let player join if not occupied
		if (this.players[n] == null || this.players[n].id == client.id) {
			this.players[n] = client;
			return n;
		}
		else
			return -1;
	}
	
	// A client disconnects. Check whether client is one of the players.
	// If yes, remove player, and release any key that the player may be
	// pressed.
	disconnect(id: string): void {
		for (let n = 0; n < this.players.length; ++n)
		{
			if (this.players[n] == null || this.players[n].id != id)
				continue ;
			console.log(`player ${n} disconnected`);
			// Assume that the disconnected player no longer presses the key
			for (const key of this.game.control_keys[n]) {
				KeyPressMonitorBase.delete(key);
			}
			this.players[n] = null;
		}
	}
	
	/* Client 'id' requests to start game. Server checks whether client
	 * is a player. */
	start(id: string) {
		for (let n = 0; n < this.players.length; ++n) {
			if (this.players[n] == null || this.players[n].id != id)
				continue ;
			this.game.start(n);	// game records that that player is ready
			console.log(`Player ${n} pressed start.`)
			this.game.control(KeyPressMonitorBase.keypress);
			this.server.emit('game_state', this.game.get_state());
		}
	}
	
	// server receives key down event
	keyDown(id: string, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.players.length; ++n) {
			if (this.players[n].id == id
					&& this.game.control_keys[n].indexOf(key) != -1) {
				console.log(`Received keydown ${key} from player ${n}`);
				KeyPressMonitorBase.add(key);
				this.game.control(KeyPressMonitorBase.keypress);
				this.server.emit('game_state', this.game.get_state());
				break ;
			}
		}
	}
	
	// server receives key up event
	keyUp(id: string, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.players.length; ++n) {
			if (this.players[n].id == id
					&& this.game.control_keys[n].indexOf(key) != -1) {
				console.log(`Received keyup ${key} from player ${n}`);
				KeyPressMonitorBase.delete(key);
				this.game.control(KeyPressMonitorBase.keypress);
				this.server.emit('game_state', this.game.get_state());
				break ;
			}
		}
	}
	
	// game has ended, so remove existing players from game
	onGameEnd(): void {
		console.log("Game has ended!");
		for (let i = 0; i < this.players.length; ++i)
			if (this.players[i] != null)
				this.players[i].emit('unjoin', i);	// unjoin as player i
	}
}
