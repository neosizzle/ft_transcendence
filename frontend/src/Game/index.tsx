import React from 'react';
import io, { Socket } from 'socket.io-client';

import Canvas from './Canvas';
import KeyPressMonitor from '../common/game/KeyPressMonitor';


class Game extends React.Component {
	player = -1;
	keypress?: KeyPressMonitor | null = null;
	
	render() {
		const socket: Socket = io("http://localhost:3001/game");  // change for production
		// 'connect' event is fired upon connection the the Namespace
		socket.on('connect', () => {
			console.log('Connected');
			// keypress monitor is created if connection is made
			this.keypress = KeyPressMonitor.get_instance(socket);
		});
		
		socket.on('identity', (n: number) => {
			console.log(`I am player ${n}`);
			this.player = n;
		})
		
		socket.on('exception', function(data) {
			console.log('event', data);
		});
		
		socket.on('disconnect', () => {
			console.log('Disconnected');
			
			// keypress monitor is deleted if connection dropped
			if (this.keypress != null)
			{
				delete this.keypress;
				this.keypress = null;
			}
		});
		
		return <Canvas width={400} height={300} style={{border: "1px solid black"}}/>;
	}
}

export default Game;
