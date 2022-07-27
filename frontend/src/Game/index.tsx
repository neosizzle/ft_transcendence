import React from 'react';
import io from 'socket.io-client';

import { Canvas } from './Canvas';


class Game extends React.Component {
	render() {
		const socket = io("ws://localhost:4896"); 
		
		socket.on("hello from server", (...args) => {
			console.log("Connected to server.");
		});
		
		return <Canvas width={400} height={300} style={{border: "1px solid black"}}/>;
	}
}

export default Game;
