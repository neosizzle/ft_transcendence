import React from 'react';
import io, { Socket } from 'socket.io-client';

import Canvas from './Canvas';
import Pong, { GameInterface, GameState } from '../common/game/Pong';
import KeyPressMonitor from '../common/game/KeyPressMonitor';


/*
This class is the GameClient.
There are possibly two types of clients: player or spectator.
Players can control their own game, spectators can only view the game.
*/
class Game extends React.Component {
	player: Set<number> = new Set<number>();
	socket: Socket;
	keypress: KeyPressMonitor | null = null;
	game: GameInterface;
	latency = 0;
	
	constructor(props: any) {
		super(props);
		this.game = new Pong(400, 300, 2);
		this.socket = io("http://localhost:3001/game");  // change for production
		this.socket_handlers();	// initialise socket message handlers
		
		// calculate connection latency every 1 second
		setInterval(() => {
			const start = Date.now();
			this.socket.emit("ping", () => {
				this.latency = (Date.now() - start) / 2;
			});
		}, 1000);
	}
	
	// define handler for socket messages
	socket_handlers() {
		// 'connect' event is fired upon connection the the Namespace
		this.socket.on('connect', () => {
			console.log('Connected');
			// keypress monitor is created if connection is made
			this.keypress = KeyPressMonitor.get_instance(
								this.onKeyDown.bind(this),
								this.onKeyUp.bind(this));
		});
		
		this.socket.on('game_state', (game_state: GameState) => {
			this.game.set_state(this.latency, game_state);
		});
		
		this.socket.on('exception', function(data) {
			console.log('event', data);
		});
		
		this.socket.on('disconnect', () => {
			console.log('Disconnected');
			
			// keypress monitor is deleted if connection dropped
			if (this.keypress != null) {
				this.keypress = null;
			}
		});
	}
	
	// callback function to be called by KeyPressMonitor class
	onKeyDown(key: KeyboardEvent["key"]): void {
		// do nothing if not a player
		if (this.player.size == 0)
			return ;
		// pass the current input to the game
		this.socket.emit('keyDown', key);
		if (this.game != null)
			this.game.control(KeyPressMonitor.keypress);
	}
	
	// callback function to be called by KeyPressMonitor class
	onKeyUp(key: KeyboardEvent["key"]): void {
		// do nothing if not a player
		if (this.player.size == 0)
			return ;
		this.socket.emit('keyUp', key);
		// pass the current input to the game
		this.game.control(KeyPressMonitor.keypress);
	}
	
	joinClick(n: number): void {
		this.socket.emit("join", n, (n: number) => {
			if (n == -1)
				return ;
			console.log(`I am player ${n}`);
			this.game.set_player(n);
			this.player.add(n);
		});
	}
	
	render() {
		return <Canvas
			width={400}
			height={300}
			style={{border: "1px solid black"}}
			game={this.game}
			joinClick={this.joinClick.bind(this)}
			/>;
	}
}

export default Game;
