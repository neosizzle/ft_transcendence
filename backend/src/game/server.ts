import { Server } from 'socket.io';

import Pong, { GameInterface } from '../common/game/Pong';
import { KeyPressMonitorBase } from '../common/game/KeyPressMonitor';


export default class GameServer {
	server: Server;
	players: string[] = ["", ""];	// records id of the players
	game: GameInterface;
	
	constructor(server: Server) {
		this.server = server;
		this.game = new Pong(400, 300, 2);
		// this.gameLoop(); 
	}
	
	// async gameLoop() {
	// 	await this.game.update();
	// 	this.gameLoop();
	// }
	
	/* client 'id' attemps to join as player 'n'. Return 'n' upon
	 * success, or -1 upon failure. */ 
	connect(id: string, n: number): number {
		// let player join if not occupied
		if (this.players[n] == "" || this.players[n] == id) {
			this.players[n] = id
			return n;
		}
		else
			return -1;
	}
	
	// player disconnects
	disconnect(id: string): void {
		for (let player = 0; player < this.players.length; ++player)
		{
			if (this.players[player] != id)
				continue ;
			console.log(`player ${player} disconnected`);
			// Assume that the disconnected player no longer presses the key
			for (let n = 0; n < this.players.length; ++n) {
				if (this.players[n] == id) {
					for (const key of this.game.control_keys[n]) {
						KeyPressMonitorBase.delete(key);
					}
				}
			}
			this.players[player] = "";
		}
	}
	
	/* Client 'id' requests to start game. Server checks whether client
	 * is a player. */
	start(id: string) {
		console.log('Active players', this.players);
		for (let n = 0; n < this.players.length; ++n) {
			if (this.players[n] == id) {
				this.game.start(n);	// game records that that player is ready
			}
		}
	}
	
	keyDown(id: string, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.players.length; ++n) {
			if (this.players[n] == id
					&& this.game.control_keys[n].indexOf(key) != -1) {
				console.log(`Received keydown ${key} from player ${n}`);
				KeyPressMonitorBase.add(key);
				break ;
			}
		}
	}
	
	keyUp(id: string, key: KeyboardEvent["key"]) {
		for (let n = 0; n < this.players.length; ++n) {
			if (this.players[n] == id
					&& this.game.control_keys[n].indexOf(key) != -1) {
				console.log(`Received keyup ${key} from player ${n}`);
				KeyPressMonitorBase.delete(key);
				break ;
			}
		}
	}
	
}