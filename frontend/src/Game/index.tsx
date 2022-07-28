import React from 'react';
import io from 'socket.io-client';

import { Canvas } from './Canvas';


class Game extends React.Component {
	render() {
		const socket = io("http://localhost:3001");  // change for production
		
		// 'connect' event is fired upon connection the the Namespace
		socket.on('connect', function() {
			console.log('Connected');
	
			socket.emit('events', { test: 'test' });
			socket.emit('identity', 0, (response: any) =>
				console.log('Identity:', response),
			);
		});
		
		socket.on('events', function(data) {
			console.log('event', data);
		});
		
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
