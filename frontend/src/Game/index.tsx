import React from 'react';
import io, { Socket } from 'socket.io-client';

import Canvas from './Canvas';
import Pong, { GameInterface } from '../common/game/Pong';
import KeyPressMonitor from '../common/game/KeyPressMonitor';


/*
This class is the GameClient.
There are possibly two types of clients: player or spectator.
Players can control their own game, spectators can only view the game.
*/
class Game extends React.Component {
	player = -1;	// -1 for spectator, 0 and above for player number
	socket: Socket;
	keypress: KeyPressMonitor | null = null;
	game: GameInterface;
	
	constructor(props: any) {
		super(props);
		this.game = new Pong(400, 300, 2);
		this.socket = io("http://localhost:3001/game");  // change for production
		this.socket_handlers();	// initialise socket message handlers
	}
	
	// define handler for socket messages
	socket_handlers() {
		// 'connect' event is fired upon connection the the Namespace
		this.socket.on('connect', () => {
			console.log('Connected');
			// keypress monitor is created if connection is made
			this.keypress = KeyPressMonitor.get_instance(
				this.onKeyDown.bind(this), this.onKeyUp.bind(this));
		});
		
		// receives the player number from server
		this.socket.on('identity', (n: number) => {
			console.log(`I am player ${n}`);
			this.game.set_player(n);
			this.player = n;
		})
		
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
	onKeyDown(): void {
		// do nothing if not a player
		if (this.player < 0)
			return ;
		// pass the current input to the game
		if (this.game != null)
			this.game.control(KeyPressMonitor.keypress);
	}
	
	// callback function to be called by KeyPressMonitor class
	onKeyUp(): void {
		// do nothing if not a player
		if (this.player < 0)
			return ;
		// pass the current input to the game
		this.game.control(KeyPressMonitor.keypress);
	}
	
	render() {
		return <Canvas
			width={400}
			height={300}
			style={{border: "1px solid black"}}
			game={this.game}
			/>;
	}
}

export default Game;
