import React from 'react';
import io from 'socket.io-client';

import { Canvas } from './Canvas';


class Game extends React.Component {
	render() {
		const socket = io("http://localhost:3001/game");  // change for production
		
		// 'connect' event is fired upon connection the the Namespace
		socket.on('connect', () => {
			console.log('Connected');
		});
		
		socket.on('identity', (n: number) => {
			console.log(`I am player ${n}`);
		})
		
		socket.on('exception', function(data) {
			console.log('event', data);
		});
		
		socket.on('disconnect', function() {
			console.log('Disconnected');
		});
		
		return <Canvas width={400} height={300} style={{border: "1px solid black"}}/>;
	}
}

export default Game;
