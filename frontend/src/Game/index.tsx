import React from 'react';
import io, { Socket } from 'socket.io-client';

import Canvas from './Canvas';
import KeyPressMonitor from '../common/game/KeyPressMonitor';


/*
This class is the GameClient.
There are possibly two types of clients: player or spectator
*/
class Game extends React.Component {
	player = -1;	// -1 for spectator, 0 and above for player number
	socket: Socket;
	keypress: KeyPressMonitor | null = null;
	
	constructor(props: any) {
		super(props);
		this.socket = io("http://localhost:3001/game");  // change for production
		
		// 'connect' event is fired upon connection the the Namespace
		this.socket.on('connect', () => {
			console.log('Connected');
			// keypress monitor is created if connection is made
			this.keypress = KeyPressMonitor.get_instance(this.socket);
		});
		
		// receives the player number from server
		this.socket.on('identity', (n: number) => {
			console.log(`I am player ${n}`);
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
	
	render() {
		return <Canvas width={400} height={300} style={{border: "1px solid black"}}/>;
	}
}

export default Game;
